-module(erlide_builder_rebar).

-export([
         build/2,
         clean/1,
         dialyze/1,
         eunit/1,
         doc/1
        ]).

-record(project_info, {rootDir, sourceDirs=[], includeDirs=[], outDir="ebin", opts=[], min_otp_vsn=".*", libs=[]}).

-spec build(atom(), list()) -> list() | error | timeout.
build(Kind, ProjProps) when is_atom(Kind), is_record(ProjProps, project_info) ->
    c:cd(ProjProps#project_info.rootDir),

    Cmds0 = ["compile", "eunit", "compile_only=true"],
    Cmds = case Kind of
               full ->
                   ["clean" | Cmds0];
               _ ->
                   Cmds0
           end,

    rebar(ProjProps, ["-C", "rebar.config" | Cmds]).

clean(ProjProps) when is_record(ProjProps, project_info) ->
    c:cd(ProjProps#project_info.rootDir),
    rebar(ProjProps, ["-C", "rebar.config", "clean"]).

dialyze(ProjProps) when is_record(ProjProps, project_info) ->
    c:cd(ProjProps#project_info.rootDir),
    rebar(ProjProps, ["-C", "rebar.config", "dialyze"]).

eunit(ProjProps) when is_record(ProjProps, project_info) ->
    c:cd(ProjProps#project_info.rootDir),
    rebar(ProjProps, ["-C", "rebar.config", "eunit"]).

doc(ProjProps) when is_record(ProjProps, project_info) ->
    c:cd(ProjProps#project_info.rootDir),
    rebar(ProjProps, ["-C", "rebar.config", "doc"]).


%%%

rebar(ProjProps=#project_info{sourceDirs=SrcDirs, outDir=OutDir}, Ops) ->
    with_config_file(ProjProps,
                     fun() ->
                             do_rebar(SrcDirs, OutDir, Ops)
                     end).


do_rebar(Srcs, Out, Ops) when is_list(Ops) ->
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
            ok;
        K:E ->
            erlide_log:logp("ERROR: rebar crashed with ~p", [{K, E}]),
            error
    end,
    group_leader(Leader, Self),
    NewLeader ! stop,
    receive
        {NewLeader, M} ->
            M
    after 100 ->
        timeout
    end.

leader(Parent, Result) ->
    receive
        stop ->
            Text = lists:reverse(Result),
            Parent ! {self(), erlide_builder_messages:parse(Text)},
            ok;
        {io_request, From, ReplyAs, Msg}=_M ->
            From ! {io_reply, ReplyAs, ok},
            Msg1 = handle(Msg),
            case Msg1 of
                none ->
                    leader(Parent, Result);
                M when is_tuple(M) ->
                    leader(Parent, Result);
                _ ->
                    leader(Parent, Result ++ Msg1)
            end;
        _M ->
            leader(Parent, Result)
    end.

handle({put_chars, _Enc, io_lib, format, ["ERROR: "++_, _]=Args}) ->
    [erlang:apply(io_lib, format, Args)];
handle({put_chars, io_lib, format, ["ERROR: "++_, _]=Args}) ->
    [erlang:apply(io_lib, format, Args)];
handle({put_chars, _Enc, io_lib, format, ["~s", Args]}) ->
    lists:append(Args);
handle({put_chars, io_lib, format, ["~s", Args]}) ->
    lists:append(Args);
handle({put_chars, _Enc, io_lib, format, ["Compiled ~s\n", Args]}) ->
    {compiled, Args};
handle({put_chars, io_lib, format, ["Compiled ~s\n", Args]}) ->
    {compiled, Args};
handle({put_chars, _Enc, io_lib, format, ["Skipped ~s\n", Args]}) ->
    {skipped, Args};
handle({put_chars, io_lib, format, ["Skipped ~s\n", Args]}) ->
    {skipped, Args};
handle(_Msg) ->
    erlide_log:log({unexpected, _Msg}),
    none.

with_config_file(ProjProps, Fun) ->
    case filelib:is_regular("rebar.config") of
        true ->
            Fun();
        _ ->
            file:write_file("rebar.config", create_rebar_config(ProjProps)),
            Result = Fun(),
            file:delete("rebar.config"),
            Result
    end.

with_app_file(SrcDir, EbinDir, Fun) ->
    case filelib:wildcard("**/*.app.{src,src.script}") of
        [] ->
            %% we try to use a file name not likely to exist
            App = get_dummy_app_name("./"),
            File = SrcDir++"/"++App++".app.src",

            file:write_file(File, "{application, '"++App++"', [{vsn,\"0\"}]}."),

            Result = Fun(),

            file:delete(File),
            file:delete(EbinDir++"/"++App++".app"),

            Result;
        _ ->
            Fun()
    end.

get_dummy_app_name(Dir) ->
    case filelib:is_regular(Dir++"src/___dummy.app.src") orelse
             filelib:is_regular(Dir++"src/___dummy.app.src.script") of
        true ->
            "___dummy___";
        false->
            "___dummy"
    end.

create_rebar_config(#project_info{rootDir = RootDir,
                                  min_otp_vsn=MinOtpVsn,
                                  sourceDirs=Srcs,
                                  includeDirs=Incs,
                                  opts=Opts,
                                  libs=Libs}) ->
    IncOpts = [{i, I} || I<-Incs],
    AllOpts = [{src_dirs, Srcs}]++IncOpts++Opts,
    Config = io_lib:format("{require_min_otp_vsn, \"~s\"}.\n"
                           "{erl_opts, ~p}.\n"
                           "{lib_dirs, ~p}.\n"
                           "",
                           [MinOtpVsn, AllOpts, Libs]),
    erlide_log:log({generated_rebar_config, RootDir, lists:flatten(Config)}),
    Config.
