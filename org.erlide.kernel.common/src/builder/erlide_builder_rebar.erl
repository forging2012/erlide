-module(erlide_builder_rebar).

-export([
         build/1,
         build_eunit/1,
         clean/1,
         eunit/1,
         doc/1,
         xref/1,
         common_test/1
        ]).

-include("erlide_builder_rebar.hrl").

build(ProjProps) ->
    rebar(ProjProps, ["-vvv", "-k", "compile"]).

build_eunit(ProjProps) ->
    rebar(ProjProps, ["-vvv", "-k", "eunit", "compile_only=true"]).

clean(ProjProps) ->
    rebar(ProjProps, ["-vv", "clean"]).

eunit(ProjProps) ->
    rebar(ProjProps, ["-vv", "-k", "eunit"]).

doc(ProjProps) ->
    rebar(ProjProps, ["-vv", "-k", "doc"]).

xref(ProjProps) ->
    rebar(ProjProps, ["-vv", "-k", "xref"]).

common_test(ProjProps) ->
    rebar(ProjProps, ["-vv", "-k", "ct"]).


%%%

rebar(ProjProps=#project_info{rootDir=RootDir, sourceDirs=SrcDirs, outDir=OutDir}, Ops) ->
    erlide_builder_rebar_server:start(fun(Event)->
                                              erlide_jrpc:event(builder, Event)
                                      end),
    {ok, OldCwd} = file:get_cwd(),
    try
        file:set_cwd(RootDir),
        TmpDir = ProjProps#project_info.tmpDir,
        with_config_file(ProjProps,
                         fun(ConfigFile) ->
                                 rebar(SrcDirs, OutDir, TmpDir, Ops++["-f", ConfigFile])
                         end)
    after
        file:set_cwd(OldCwd),
        erlide_builder_rebar_server:stop()
    end.


rebar(Srcs, Out, TmpDir, Ops) when is_list(Ops) ->
    with_app_file(Srcs,
                  Out,
                  TmpDir,
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
        erlide_log:log(Ops),
        rebar:run(Ops)
    catch
        throw:rebar_abort ->
            erlide_log:log("rebar aborted"),
            ok;
        K:E ->
            erlide_log:logp("ERROR: rebar crashed with ~p", [{K, E, erlang:get_stacktrace()}]),
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
            Return = lists:reverse(Result),
            Parent ! {done, Return},
            %% erlide_jrpc:event(builder, done),
            ok;
        {io_request, From, ReplyAs, Msg}=_M ->
            From ! {io_reply, ReplyAs, ok},
            Msg1 = handle(Msg),
            case Msg1 of
                none ->
                    leader(Parent, Result);
                {messages, Msgs} ->
                    %% erlide_jrpc:event(builder, erlide_builder_messages:parse(Msgs)),
                    erlide_builder_rebar_server:event({messages, erlide_builder_messages:parse(Msgs)}),
                    leader(Parent, Result ++ Msgs);
                _ ->
                    %% erlide_jrpc:event(builder, Msg1),
                    erlide_builder_rebar_server:event(Msg1),
                    leader(Parent, Result)
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
    {messages, [erlang:apply(io_lib, format, Args)]};
handle_aux(["~s", _]=Args) ->
    {messages, [erlang:apply(io_lib, format, Args)]};
handle_aux(["Compiled ~s\n", [File]]) ->
    {compiled, File};
handle_aux(["Compiling ~s failed:\n", [File]]) ->
    {compiled, File};
handle_aux(["INFO:  Skipped ~s\n", [File]]) ->
    {skipped, File};
handle_aux(["DEBUG: files to compile: ~s ~p~n", [Tag, Num]]) ->
    {total, Tag, Num};
handle_aux(["==> ~s (~s)\n", [Project, Operation]]) ->
    {start, Operation, Project};
handle_aux(["DEBUG: ~s:~n~p~n", ["Dependencies of "++File, Deps]]) ->
    {dependencies, File, Deps};
handle_aux(["DEBUG: ~s: ~p~n", ["Dependencies of "++_, []]]) ->
    none;
handle_aux(["DEBUG: ~s:~n~p~n",["Files dependent on "++File, Deps]]) ->
    {dependents, File, Deps};
handle_aux(["DEBUG: ~s: ~p~n",["Files dependent on "++_, []]]) ->
    none;

handle_aux(["~sWarning: ~s is undefined function (Xref)\n",[Loc, MFA]]) ->
    {undefined_function, Loc, MFA};
handle_aux(["~sWarning: ~s calls undefined function ~s (Xref)\n",[Loc, SrcMFA, TargetMFA]]) ->
    {undefined_function_call, Loc, SrcMFA, TargetMFA};
handle_aux(["~sWarning: ~s is unused export (Xref)\n",[Loc, MFA]]) ->
    {unused_export, Loc, MFA};
handle_aux(["~sWarning: ~s is unused local function (Xref)\n",[Loc, MFA]]) ->
    {unused_local_function, Loc, MFA};

handle_aux(["======================== EUnit ========================\n",[]]) ->
    eunit_tests;
handle_aux(["~s\n",[Bin]]) when is_binary(Bin) ->
    "module '"++Module = binary_to_list(Bin),
    {eunit, list_to_atom(lists:reverse(tl(lists:reverse(Module))))};
handle_aux(["~s:~s ~s~s...", [A, B, C, D]]) ->
    {eunit_msg, A, lists:flatten(B), C, D};
handle_aux(["  Failed: ~w.  Skipped: ~w.  Passed: ~w.\n", [Failed, Skipped, Passed]]) ->
    {eunit_result, Failed, Skipped, Passed};
handle_aux(["*failed*\n~s", Text]) ->
    {failed, lists:flatten(Text)};

handle_aux(_Msg) ->
    %% erlide_log:log({unexpected, _Msg}),
    none.

with_config_file(ProjProps, Fun) ->
    TmpDir = ProjProps#project_info.tmpDir,
    case filelib:is_regular(TmpDir++"/.erlide.rebar.config") of
        true ->
            file:delete(TmpDir++"/rebar.config"),
            file:delete(TmpDir++"/.erlide.rebar.config");
        false ->
            ok
    end,

    case filelib:is_regular("rebar.config") of
        true ->
            Fun("rebar.config");
        _ ->
            try
                file:write_file(TmpDir++"/.erlide.rebar.config", ""),
                ok = file:write_file(TmpDir++"/rebar.config", create_rebar_config(ProjProps))
            catch
                K:E ->
                    erlide_log:log({"ERROR: could not create rebar.config", TmpDir, {K,E}})
            end,
            try
                Fun(TmpDir++"/rebar.config")
            after
                file:delete(TmpDir++"/rebar.config"),
                file:delete(TmpDir++"/.erlide.rebar.config")
            end
    end.

with_app_file(SrcDir, EbinDir, TmpDir, Fun) ->
    case filelib:is_regular(TmpDir++"/.erlide.rebar.app") of
        true ->
            file:delete("___dummy___.app.src"),
            file:delete("___dummy___.app.src.script"),
            file:delete(TmpDir++"/.erlide.rebar.app");
        false ->
            ok
    end,

    case filelib:wildcard("**/*.app.{src,src.script}") of
        [] ->
            %% we try to use a file name not likely to exist
            File = lists:flatten(SrcDir++"/___dummy___.app.src"),
            try
                file:write(TmpDir++"/.erlide.rebar.app", ""),
                ok = file:write_file(File, "{application, '___dummy___', [{vsn,\"0\"}]}.")
            catch
                K:E ->
                    erlide_log:log({"ERROR: could not create .app.src", {K,E}})
            end,
            %% we try to run even if creation of app.src failed,
            %% there might be some magic at work
            try
                Fun()
            after
                file:delete(File),
                file:delete(EbinDir++"/___dummy___.app"),
                file:delete(TmpDir++"/.erlide.rebar.app")
            end;
        _ ->
            Fun()
    end.

create_rebar_config(#project_info{%% rootDir=RootDir,
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
    %% erlide_log:log({generated_rebar_config, RootDir, lists:flatten(Config)}),
    Config.
