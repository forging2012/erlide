-module(erlide_builder_rebar).

-export([
         build/2,
         clean/1,
         eunit/1,
         doc/1
        ]).

-include("erlide_builder_rebar.hrl").

-spec build(atom(), list()) -> list() | error | timeout.
build(Kind, ProjProps) ->
    Cmds0 = ["compile", "eunit", "compile_only=true"],
    Cmds = case Kind of
               full ->
                   ["clean" | Cmds0];
               _ ->
                   Cmds0
           end,

    rebar(ProjProps, ["-vvv" | Cmds]).

clean(ProjProps) ->
    rebar(ProjProps, ["-vv", "clean"]).

eunit(ProjProps) ->
    rebar(ProjProps, ["-vv", "eunit"]).

doc(ProjProps) ->
    rebar(ProjProps, ["-vv", "doc"]).


%%%

rebar(ProjProps=#project_info{rootDir=RootDir, sourceDirs=SrcDirs, outDir=OutDir}, Ops) ->
    {ok, OldCwd} = file:get_cwd(),
    try
        c:cd(RootDir),
        with_config_file(ProjProps,
                         fun() ->
                                 rebar(SrcDirs, OutDir, Ops)
                         end)
    after
        c:cd(OldCwd)
    end.


rebar(Srcs, Out, Ops) when is_list(Ops) ->
    with_app_file(Srcs,
                  Out,
                  fun()->
                          call_rebar(Ops)
                  end).

call_rebar(Ops) ->
    application:unload(rebar),
    Leader = group_leader(),
    Self = self(),
    NewLeader = spawn(fun() -> leader(Self, []) end),
    group_leader(NewLeader, Self),
    try
        rebar:run(Ops)
    catch
        throw:rebar_abort ->
            erlide_log:log("rebar aborted"),
            ok;
        K:E ->
            erlide_log:logp("ERROR: rebar crashed with ~p", [{K, E}]),
            error
    end,
    group_leader(Leader, Self),
    NewLeader ! stop,
    receive
        {done, M} ->
            M
    after 100 ->
        timeout
    end.

leader(Parent, Result) ->
    receive
        stop ->
            Text = lists:reverse(Result),
            Parent ! {done, erlide_builder_messages:parse(Text)},
            erlide_jrpc:event(builder, done),
            ok;
        {io_request, From, ReplyAs, Msg}=_M ->
            From ! {io_reply, ReplyAs, ok},
            Msg1 = handle(Msg),
            case Msg1 of
                none ->
                    leader(Parent, Result);
                M when is_tuple(M) ->
                    erlide_jrpc:event(builder, Msg1),
                    leader(Parent, Result);
                _ ->
                    erlide_jrpc:event(builder, erlide_builder_messages:parse(Msg1)),
                    leader(Parent, Result ++ Msg1)
            end;
        _M ->
            leader(Parent, Result)
    end.

handle({put_chars, _Enc, io_lib, format, Args}) ->
    handle_aux(Args);
handle({put_chars, io_lib, format, Args}) ->
    handle_aux(Args);
handle(_Msg) ->
    none.

handle_aux(["ERROR: "++_, _]=Args) ->
    [erlang:apply(io_lib, format, Args)];
handle_aux(["~s", Args]) ->
    lists:append(Args);
handle_aux(["Compiled ~s\n", [File]]) ->
    {compiled, File};
handle_aux(["INFO:  Skipped ~s\n", [File]]) ->
    {skipped, File};
handle_aux(["DEBUG: files to compile: ~s ~p~n", [Tag, Num]]) ->
    {total, Tag, Num};
handle_aux(["==> ~s (~s)\n", [Project, Operation]]) ->
    {start, Operation, Project};
handle_aux(_Msg) ->
    erlide_log:log({unexpected, _Msg}),
    none.

with_config_file(ProjProps, Fun) ->
    case filelib:is_regular("rebar.config") of
        true ->
            Fun();
        _ ->
            try
                ok = file:write_file("rebar.config", create_rebar_config(ProjProps))
            catch
                K:E ->
                    erlide_log:log({"ERROR: could not create rebar.config", {K,E}})
            end,
            try
                Fun()
            after
                file:delete("rebar.config")
            end
    end.

with_app_file(SrcDir, EbinDir, Fun) ->
    case filelib:wildcard("**/*.app.{src,src.script}") of
        [] ->
            %% we try to use a file name not likely to exist
            App = get_dummy_app_name("."),
            File = lists:flatten(SrcDir++"/"++App++".app.src"),
            try
                ok = file:write_file(File, "{application, '"++App++"', [{vsn,\"0\"}]}.")
            catch
                K:E ->
                    erlide_log:log({"ERROR: could not create rebar.config", {K,E}})
            end,
            try
                Fun()
            after
                file:delete(File),
                file:delete(EbinDir++"/"++App++".app")
            end;
        _ ->
            Fun()
    end.

get_dummy_app_name(Dir) ->
    case filelib:is_regular(Dir++"/src/___dummy.app.src") orelse
             filelib:is_regular(Dir++"/src/___dummy.app.src.script") of
        true ->
            "___dummy___";
        false->
            "___dummy"
    end.

create_rebar_config(#project_info{rootDir=RootDir,
                                  min_otp_vsn=MinOtpVsn,
                                  sourceDirs=Srcs,
                                  includeDirs=Incs,
                                  outDir=OutDir,
                                  opts=Opts,
                                  libs=Libs}) ->
    IncOpts = [{i, I} || I<-Incs],
    AllOpts = [{outdir, OutDir}, {src_dirs, Srcs}]++IncOpts++Opts,
    Config = io_lib:format("{require_min_otp_vsn, \"~s\"}.\n"
                           "{erl_opts, ~p}.\n"
                           "{lib_dirs, ~p}.\n"
                           "{eunit_opts, [verbose]}.\n"
                           "",
                           [MinOtpVsn, AllOpts, Libs]),
    erlide_log:log({generated_rebar_config, RootDir, lists:flatten(Config)}),
    Config.
