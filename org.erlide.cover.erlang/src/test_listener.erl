%% Author: Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
%% Created: 20-10-2011
%% Description: eunit test listener 
%% (listens for test results and forwards them to ErlIde)
%%----------------------------------------------
-module(test_listener).
-behaviour(eunit_listener).

%%----------------------------------------------
%% Include files
%%----------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("test.hrl").


%%----------------------------------------------
%% Exported Functions
%%----------------------------------------------

-export([start/0, start/1]).

-export([init/1, handle_begin/3, handle_end/3, handle_cancel/3,
     terminate/2]).


%%----------------------------------------------
%% API Functions
%%----------------------------------------------

start() ->
    start([]).

start(Options) ->
    eunit_listener:start(?MODULE, Options).

init(_Options) ->
	io:format("inite"),
    %St = #state{verbose = proplists:get_bool(verbose, Options)},
	St = [],
    receive
    {start, _Reference} ->
        %if St#state.verbose -> print_header();
        %   true -> ok
        %end,
        St
    end.

terminate({ok, Data}, _St) ->
	io:format("blab"),
    Pass = proplists:get_value(pass, Data, 0),
    Fail = proplists:get_value(fail, Data, 0),
    Skip = proplists:get_value(skip, Data, 0),
    Cancel = proplists:get_value(cancel, Data, 0),
	io:format("test terminate: ~p ~p ~p ~p ~n", [Pass, Fail, Skip, Cancel]),
    erlide_jrpc:event(?TEVENT, #result{pass = Pass,
									  fail = Fail,
									  skip = Skip,
									  cancel = Cancel}),
    sync_end(ok);
terminate({error, Reason}, _St) ->
	io:format("terminate with error ~p~n", [Reason]),
	erlide_jrpc:event(?TEVENT, {end_with_error, blab}),
    sync_end(error).

sync_end(Result) ->
    receive
    {stop, Reference, ReplyTo} ->
        ReplyTo ! {result, Reference, Result},
        ok
    end.

handle_begin(group, Data, St) ->
	io:format("group begin: ~p~n", [Data]),
    Desc = proplists:get_value(desc, Data),
	Group = case St of
				[H | _T] -> make_list(H);
				_ ->
					""
			end,
	if Desc =/= "", Desc =/= undefined  ->
		   	erlide_jrpc:event(?TEVENT, {gbegin, Group, make_list(Desc)}),
			[Desc | St];
	   true ->
		   St
	end;
handle_begin(test, Data, St) ->	
	io:format("test begin: ~p~n", [Data]),
	io:format("test state: ~p~n", [St]),
	Desc = proplists:get_value(desc, Data),
    Line = proplists:get_value(line, Data, 0),
	Group = case St of
				[H | _T] -> make_list(H);
				_ ->
					""
			end,
    erlide_jrpc:event(?TEVENT, {tbegin, Group, Desc, Line}),
    St.

handle_end(group, Data, St) ->
	io:format("group end: ~p~n", [Data]),
    Desc = proplists:get_value(desc, Data),
	if Desc =/= "", Desc =/= undefined ->
    		Time = proplists:get_value(time, Data),
			erlide_jrpc:event(?TEVENT, {gend, make_list(Desc), Time}),
    		lists:delete(Desc, St);
		true -> 
			St
    end;
handle_end(test, Data, St) ->
	io:format("test end ~p~n", [Data]),
	Desc = proplists:get_value(desc, Data),
   	Line = proplists:get_value(line, Data, 0),
	Source = case proplists:get_value(source, Data) of 
				 {SM, SF, SA} ->
					 {SM, SF, SA};
				 _ ->
					 {[], [], 0}
			 end,
	Group = case St of
				[H | _T] -> make_list(H);
				_ ->
					""
			end,
    case proplists:get_value(status, Data) of
    ok ->
		Time = proplists:get_value(time, Data, 0),
		erlide_jrpc:event(?TEVENT, {tend, Group, Desc, Line, Source, Time});
    {error, Exception} ->
        erlide_jrpc:event(?TEVENT, {error, Group, Desc, Line, Source, Exception} );
	{skipped, {module_not_found, M}} ->
		erlide_jrpc:event(?TEVENT, {skipped, module_not_found, Group, Desc, Line, Source, M});
	{skipped, {no_such_function, {M, F, A}}}  ->
		erlide_jrpc:event(?TEVENT, {skipped, no_such_function, Group, Desc, Line, Source, {M, F, A}})
    end,
	St.

handle_cancel(group, Data, St) ->
	io:format("group canceled ~p~n", [Data]),
	Desc = proplists:get_value(desc, Data),
	if Desc =/= "", Desc =/= undefined ->
    		Res = cancel_info(Data),
			erlide_jrpc:event(?TEVENT, {gcanceled, make_list(Desc), Res}),
			lists:delete(Desc, St);
	   true ->
		   St
	end;
handle_cancel(test, Data, St) ->
	io:format("test canceled ~p~n", [Data]),
	Desc = proplists:get_value(desc, Data),
   	Line = proplists:get_value(line, Data, 0),
	Res = cancel_info(Data),
	Source = case proplists:get_value(source, Data) of 
				 {SM, SF, SA} ->
					 {SM, SF, SA};
				 _ ->
					 {[], [], 0}
			 end,
	Group = case St of
				[H | _T] -> make_list(H);
				_ ->
					""
			end,
	erlide_jrpc:event(?TEVENT, {tcanceled, Group, Desc, Line, Res, Source}),
    St.

%%--------------
%% Helpers
%%--------------


cancel_info(Data) ->
	case proplists:get_value(reason, Data) of
    	undefined ->
			"skipped";
    	timeout ->
        	"timed out";
        {startup, Reason} ->
			io_lib:format("could not start test process\n::~P\n\n",
          [Reason, 15]);
		{blame, _SubId} ->
			"cancelled because of subtask";
		{exit, Reason} ->
			io_lib:format("unexpected termination of test process\n::~P\n\n",
          [Reason, 15]);
		{abort, _Reason} ->
			"aborted"		%%!!! 
    end.

make_list(H) when is_binary(H) ->
	binary_to_list(H);
make_list(H) ->
	H.





