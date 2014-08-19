-module(erlide_builder_messages_tests).

-include_lib("eunit/include/eunit.hrl").

convert_test_() ->
    [
     ?_assertEqual({"f.erl", 1, "ok", error},
                   erlide_builder_messages:parse_error("f.erl:1:ok")),
     ?_assertEqual({"f.erl", 1, "ok", warning},
                   erlide_builder_messages:parse_error("f.erl:1: Warning: ok")),
     ?_assertEqual({"f.c", 1, "ok", error},
                   erlide_builder_messages:parse_error("f.c:1:ok")),
     ?_assertEqual({"f.zrl", -1, "da:f", error},
                   erlide_builder_messages:parse_error("f.zrl:none:da:f")),
     ?_assertEqual(not_an_error_line,
                   erlide_builder_messages:parse_error("f.zrl:ok:da:f")),
     ?_assertEqual(not_an_error_line,
                   erlide_builder_messages:parse_error("something")),

     ?_assertEqual(not_an_error_line,
                   erlide_builder_messages:parse_error(" f.zrl:1: daf")),
     ?_assertEqual(not_an_error_line,
                   erlide_builder_messages:parse_error("f.erl:2")),
     ?_assertEqual(not_an_error_line,
                   erlide_builder_messages:parse_error("f.erl:2:")),

     ?_assertEqual({project, -1, "ok", error},
                   erlide_builder_messages:parse_error("ERROR: ok")),

     ?_assertEqual([{"f.erl", 1, "ok", error},
                    {"g.hrl", 2, "ok", error},
                    {"f.erl", 3, "nok", error}
                    ],
                   erlide_builder_messages:parse(["f.erl:1:ok",
                                                  "gaga",
                                                  {skipped, "x"},
                                                  "g.hrl:2:ok",
                                                  "f.erl:3:  nok"]))
    ].

