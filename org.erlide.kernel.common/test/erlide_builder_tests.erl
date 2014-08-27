-module(erlide_builder_tests).

-include_lib("eunit/include/eunit.hrl").

-include("erlide_builder_rebar.hrl").

rebar_test_() ->
    [
     ?_assertNotMatch(non_existing, code:which(rebar)),
     ?_assertEqual("2.5.1", rebar:version())
    ].

build_project_test_() ->
    [
     ?_test(build_builders_project())
    ].

build_builders_project() ->
    RootDir = "../../org.erlide.ui.tests/projects/builders",
    erlide_builder_rebar:build(full,
                               #project_info{
                                             rootDir=RootDir,
                                             sourceDirs="src"
                                            }),
    %% check that beams and app have been created
    ok.
