-module(erlide_builder_rebar_server).

-behaviour(gen_fsm).

-export([init/1,

         init/2,
         compile/2,
         compile_1/2,
         eunit/2,
         clean/2,
         ignoring/2,
         xref/2,

         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4
        ]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
         start/1,
         stop/0,
         event/1
        ]).

%% to avoid warnings
-export([trace/3]).

-define(SERVER, ?MODULE).

start(Notifier) ->
    R = gen_fsm:start({local, ?SERVER},
                      ?MODULE,
                      Notifier,
                      [
                        %% {debug, [{install, {fun trace/3, ok}}]}
                      ]
                     ),
    R.

stop() ->
    gen_fsm:send_all_state_event(?SERVER, done).

event({start,_,_}=Event) ->
    gen_fsm:send_all_state_event(?SERVER, Event);
event(Event) ->
    gen_fsm:send_event(?SERVER, Event).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state,
        {
         notifier = fun(_) -> ok end,
         operation = none,
         messages = [],
         deps = []
        }).

-include("erlide_builder_rebar_server.hrl").

init(Notifier) ->
    {ok, init, #state{notifier = Notifier}}.

%% FSM states

init(Event, StateData) ->
    unhandled(Event, init),
    {next_state, init, StateData}.

compile({total, Tag, Num}, StateData=#state{notifier=Notifier}) ->
    Notifier({step, Tag, Num}),
    {next_state, compile_1, StateData#state{messages=[], deps=[]}};
compile(Event, StateData) ->
    unhandled(Event, compile),
    {next_state, compile, StateData}.

compile_1({total, Tag, Num}, StateData=#state{notifier=Notifier}) ->
    Notifier({step, Tag, Num}),
    {next_state, compile_1, StateData#state{messages=[], deps=[]}};
compile_1({messages, Msgs}, StateData=#state{messages=Items}) ->
    {next_state, compile_1, StateData#state{messages=Items++Msgs}};
compile_1({dependencies, File, Deps}, StateData=#state{deps=Dependencies}) ->
    {next_state, compile_1, StateData#state{deps=[{File, Deps}|Dependencies]}};
compile_1({skipped, File}, StateData=#state{notifier=Notifier}) ->
    Notifier({skipped, File}),
    {next_state, compile_1, StateData#state{messages=[]}};
compile_1({compiled, File}, StateData=#state{notifier=Notifier, messages=Items, deps=Dependencies}) ->
    Notifier({compiled, File, Items, proplists:get_value(File, Dependencies, [])}),
    {next_state, compile_1, StateData#state{messages=[]}};
compile_1(Event, StateData) ->
    unhandled(Event, compile_1),
    {next_state, compile, StateData}.

eunit({total, Tag, Num}, StateData=#state{notifier=Notifier}) ->
    Notifier({step, Tag, Num}),
    {next_state, compile_1, StateData#state{messages=[], deps=[]}};
eunit(eunit_tests, StateData=#state{notifier=Notifier}) ->
    Notifier({step, tests, 0}),
    {next_state, compile_1, StateData#state{messages=[], deps=[]}};
eunit(Event, StateData) ->
    unhandled(Event, eunit),
    {next_state, init, StateData}.

clean(Event, StateData) ->
    unhandled(Event, clean),
    {next_state, init, StateData}.

ignoring(Event, StateData) ->
    unhandled(Event, ignoring),
    {next_state, ignoring, StateData}.

xref({unused_export, Loc, MFA}, StateData=#state{notifier=_Notifier}) ->
    {next_state, xref, StateData};
xref(Event, StateData) ->
    unhandled(Event, xref),
    {next_state, init, StateData}.

handle_event({start,Operation,_Project}, _StateName, StateData=#state{notifier=Notifier}) ->
    Notifier({phase, Operation}),
    {next_state, Operation, StateData#state{operation=Operation}};
handle_event(done, _, StateData) ->
    {stop, normal, StateData};
handle_event(Event, StateName, StateData) ->
    erlide_log:log({unexpected, all_state_event, Event, StateName}),
    {next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_info(Info, StateName, StateData) ->
    erlide_log:log({info, builder_server, Info}),
    {next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StatData) ->
    erlide_log:log({terminating, builder_server, _Reason, _StateName}),
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%% ====================================================================
%% Internal functions
%% ====================================================================

trace(FuncState, {in, Msg}, {_,ProcState}) ->
    erlide_log:log({'------>TRACE', Msg, ProcState}),
    FuncState;
trace(FuncState, return, {_,ProcState}) ->
    erlide_log:log({'------<TRACE', ProcState}),
    FuncState;
trace(FuncState, Event, {_,ProcState}) ->
    erlide_log:log({'-------TRACE', Event, ProcState}),
    FuncState.

unhandled(Event, StateName) ->
    erlide_log:log({'----??-UNHANDLED', Event, StateName}),
    ok.
