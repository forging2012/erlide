-module(erlide_builder_tests).

-include_lib("eunit/include/eunit.hrl").

rebar_test_() ->
    [
     ?_assertNotMatch(non_existing, code:which(rebar)),
     ?_assertEqual("2.5.1", rebar:version())
    ].
