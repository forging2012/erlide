-module(g_tests).

-include_lib("eunit/include/eunit.hrl").

g_test_() ->
	[
	 ?_assertEqual(true, true)
	 ].
