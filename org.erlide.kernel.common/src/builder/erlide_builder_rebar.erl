-module(erlide_builder_rebar).

-export([
         build/1,
         clean/1
        ]).

build(Dir) ->
    c:cd(Dir),
    file:write_file("src/ww.app.src", "{application, ww, [{vsn,\"2\"}]}."),
    Result = rebar(["-C", "rebar.config", "compile", "-v"]),
    file:delete("src/ww.app.src"),
    file:delete("ebin/ww.app"),
    Result.

clean(Dir) ->
    c:cd(Dir),
    rebar("clean").

rebar(Ops) when is_list(Ops) ->
    application:unload(rebar),
    Leader = group_leader(),
    Self = self(),
    NewLeader = spawn(fun() -> leader(Self, []) end),
    group_leader(NewLeader, Self),
    try 
        rebar:run(Ops)
    catch 
        _:_ -> 
            error
    end,
    group_leader(Leader, Self),
    NewLeader ! stop,
    receive 
        {NewLeader, M} -> 
            M
    after 100 ->
        timeout
    end;
rebar(Op) ->
    rebar([Op]).

leader(Parent, Result) ->
    receive
        stop ->
            Parent ! {self(), Result},
            ok;
        {io_request, From, ReplyAs, Msg} ->
            From ! {io_reply, ReplyAs, ok},
            Msg1 = handle(Msg),
            case Msg1 of
                none ->
                    leader(Parent, Result);
                _ ->
                    leader(Parent, [Msg1 | Result])
            end;
        _ ->
            leader(Parent, Result)
    end.

handle({put_chars, _Enc, M, F, Args}) ->
    Msg0 = lists:flatten(erlang:apply(M, F, Args)),
    %[erlide_erlcerrors:convert_error(Msg0)];
    Msg0;
handle(_) ->
    none.

