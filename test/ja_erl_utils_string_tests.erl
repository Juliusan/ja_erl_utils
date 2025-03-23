%%%
%%% Unit tests for ja_erl_utils_string.
%%%
-module(ja_erl_utils_string_tests).
-include_lib("eunit/include/eunit.hrl").


get_integer_test_() ->
    [
        ?_assertEqual({15,         " 23"         }, ja_erl_utils_string:get_integer("15 23"         )),
        ?_assertEqual({-15,        " 23"         }, ja_erl_utils_string:get_integer("-15 23"        )),
        ?_assertEqual({0,          "-15 23"      }, ja_erl_utils_string:get_integer("0-15 23"       )),
        ?_assertEqual({15,         "anythingelse"}, ja_erl_utils_string:get_integer("15anythingelse")),
        ?_assertEqual({15,         ""            }, ja_erl_utils_string:get_integer("15"            )),
        ?_assertEqual({1234567890, ""            }, ja_erl_utils_string:get_integer("1234567890"    )),
        ?_assertError(badarg,                       ja_erl_utils_string:get_integer("+15 23"        ))
    ].


get_integer_list_test_() ->
    [
        ?_assertEqual([],                 ja_erl_utils_string:get_integer_list(""                  )),
        ?_assertEqual([23],               ja_erl_utils_string:get_integer_list("23"                )),
        ?_assertEqual([23],               ja_erl_utils_string:get_integer_list("23   "             )),
        ?_assertEqual([23],               ja_erl_utils_string:get_integer_list("    23"            )),
        ?_assertEqual([23],               ja_erl_utils_string:get_integer_list("    23   "         )),
        ?_assertEqual([15, 23],           ja_erl_utils_string:get_integer_list("15 23"             )),
        ?_assertEqual([15, 2321, 12, 24], ja_erl_utils_string:get_integer_list("15     2321 12  24")),

        ?_assertEqual([15, 23],           ja_erl_utils_string:get_integer_list("15,23"          , "," )),
        ?_assertEqual([15, 2321, 12, 24], ja_erl_utils_string:get_integer_list("15,2321,,12,24,", "," )),
        ?_assertEqual([15, 2321, 2014],   ja_erl_utils_string:get_integer_list("15, 2321, 2014",  ", "))
    ].


drop_trailing_new_line_test_() ->
    [
        ?_assertEqual("",           ja_erl_utils_string:drop_trailing_new_line("\n"          )),
        ?_assertEqual("\n",         ja_erl_utils_string:drop_trailing_new_line("\n\n"        )),
        ?_assertEqual("abcdef",     ja_erl_utils_string:drop_trailing_new_line("abcdef\n"    )),
        ?_assertEqual("abc\ndef",   ja_erl_utils_string:drop_trailing_new_line("abc\ndef\n"  )),
        ?_assertEqual("ab\ncd\nef", ja_erl_utils_string:drop_trailing_new_line("ab\ncd\nef\n")),
        ?_assertError(badarg,       ja_erl_utils_string:drop_trailing_new_line("ab\ncd\nef"  ))
    ].
