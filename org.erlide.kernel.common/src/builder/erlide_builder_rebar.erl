-module(erlide_builder_rebar).

-export([
         build/2,
         clean/1,
         dialyze/1,
         eunit/1,
         doc/1
        ]).

-spec build(atom(), list()) -> list() | error | timeout.
build(Kind, Dir) when is_atom(Kind), is_list(Dir) ->
    c:cd(Dir),
    
    Cmds0 = ["compile", "eunit", "compile_only=true"],
    Cmds = case Kind of
               full ->
                   ["clean" | Cmds0];
               _ ->
                   Cmds0
           end,
    rebar(["-C", "rebar.config" | Cmds]).

clean(Dir) ->
    c:cd(Dir),
    rebar(["-C", "rebar.config", "clean"]).

dialyze(Dir) ->
    c:cd(Dir),
    rebar(["-C", "rebar.config", "dialyze"]).

eunit(Dir) ->
    c:cd(Dir),
    rebar(["-C", "rebar.config", "eunit"]).

doc(Dir) ->
    c:cd(Dir),
    rebar(["-C", "rebar.config", "doc"]).


%%%

rebar(Ops) when is_list(Ops) ->
    with_config_file(fun() ->
                             with_app_file(fun()->
                                                   call_rebar(Ops)
                                           end)
                     end);
rebar(Op) ->
    rebar([Op]).

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

with_config_file(Fun) ->
    case filelib:is_regular("rebar.config") of
        true ->
            Fun();
        _ ->
            file:write_file("rebar.config", ""),
            Result = Fun(),
            file:delete("rebar.config"),
            Result
    end.

with_app_file(Fun) ->
    case filelib:wildcard("**/*.app.{src,src.script}") of
        [] ->
            %% TODO which directory?
            
            %% we try to use a file name not likely to exist
            App = get_dummy_app_name("./"),
            File = "src/"++App++".app.src",
            
            file:write_file(File, "{application, '"++App++"', [{vsn,\"0\"}]}."),
            
            Result = Fun(),
            
            file:delete(File),
            file:delete("ebin/"++App++".app"),
            
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
