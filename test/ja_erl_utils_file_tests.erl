%%%
%%% Unit tests for ja_erl_utils_file.
%%%
-module(ja_erl_utils_file_tests).
-include_lib("eunit/include/eunit.hrl").


read_file_test_() ->
    [
        ?_assertEqual("Single line file\n", ja_erl_utils_file:read_file(path("single_line.txt"))),
        ?_assertEqual("\n"                , ja_erl_utils_file:read_file(path("single_empty_line.txt"))),
        ?_assertEqual(
            "First line first part\n" ++
            "Second line first part\n" ++
            "Third line first part\n" ++
            "----\n" ++
            "1 line 2 part\n" ++
            "2 line 2 part\n" ++
            "3 line 2 part\n" ++
            "4 line 2 part\n",              ja_erl_utils_file:read_file(path("two_part_file.txt")))
    ].


read_only_line_test_() ->
    [
        ?_assertEqual("Single line file\n", ja_erl_utils_file:read_only_line(path("single_line.txt"))),
        ?_assertEqual("\n",                 ja_erl_utils_file:read_only_line(path("single_empty_line.txt")))
    ].


read_only_line_no_new_line_test_() ->
    [
        ?_assertEqual("Single line file", ja_erl_utils_file:read_only_line_no_new_line(path("single_line.txt"))),
        ?_assertEqual("",                 ja_erl_utils_file:read_only_line_no_new_line(path("single_empty_line.txt")))
    ].


read_lines_test_() ->
    [
        ?_assertEqual(["Single line file\n"], ja_erl_utils_file:read_lines(path("single_line.txt"))),
        ?_assertEqual(["\n"                ], ja_erl_utils_file:read_lines(path("single_empty_line.txt"))),
        ?_assertEqual([
            "4 line 2 part\n",
            "3 line 2 part\n",
            "2 line 2 part\n",
            "1 line 2 part\n",
            "----\n",
            "Third line first part\n",
            "Second line first part\n",
            "First line first part\n"
                                           ], ja_erl_utils_file:read_lines(path("two_part_file.txt")))
    ].


read_lines_no_new_line_test_() ->
    [
        ?_assertEqual(["Single line file"], ja_erl_utils_file:read_lines_no_new_line(path("single_line.txt"))),
        ?_assertEqual([""                ], ja_erl_utils_file:read_lines_no_new_line(path("single_empty_line.txt"))),
        ?_assertEqual([
            "4 line 2 part",
            "3 line 2 part",
            "2 line 2 part",
            "1 line 2 part",
            "----",
            "Third line first part",
            "Second line first part",
            "First line first part"
                                         ], ja_erl_utils_file:read_lines_no_new_line(path("two_part_file.txt")))
    ].


read_lines_to_elems_test_() ->
    LastFun = fun
        LastFun([Last|"\n"]) -> Last;
        LastFun([_|Line])    -> LastFun(Line)
    end,
    SingleFun = fun([First|Line]) -> [First, LastFun(Line)] end,
    SingleTestFun = fun(File) -> ja_erl_utils_file:read_lines_to_elems(path(File), SingleFun) end,
    F2Part1Fun = fun(Line) -> [Index, "line first part\n"] = string:split(Line, " "), Index end,
    F2Part2Fun = fun(Line) -> [Index, "line 2 part\n"] = string:split(Line, " "), erlang:list_to_integer(Index) end,
    F2Separator = "----\n",
    F2TestFun = fun(File) -> ja_erl_utils_file:read_lines_to_elems(path(File), [F2Part1Fun, F2Part2Fun], F2Separator) end,
    F3Part1Fun = fun([D1,D2,_|"\n"]) -> (D1-$0)*10 + D2 - $0 end,
    F3Part2Fun = fun([D1,_,D3|"\n"]) -> (D3-$0)*10 + D1 - $0 end,
    F3Part3Fun = fun([_,D2,D3|"\n"]) -> (D2-$0)*10 + D3 - $0 end,
    F3Separator = "===\n",
    F3TestFun = fun(File) -> ja_erl_utils_file:read_lines_to_elems(path(File), [F3Part1Fun, F3Part2Fun, F3Part3Fun], F3Separator) end,
    [
        ?_assertEqual(["Se"],                                                                   SingleTestFun("single_line.txt"               )),
        ?_assertEqual(["4t", "3t", "2t", "1t", "--",             "Tt", "St", "Ft"],             SingleTestFun("two_part_file.txt"             )),
        ?_assertEqual([            "2t", "1t", "--"                              ],             SingleTestFun("two_part_first_empty_file.txt" )),
        ?_assertEqual([                        "--", "Ft", "Ft", "Tt", "St", "Ft"],             SingleTestFun("two_part_second_empty_file.txt")),
        ?_assertEqual([                        "--"                              ],             SingleTestFun("two_part_empty_file.txt"       )),
        ?_assertEqual(["60", "59", "48", "37", "26", "15", "==", "31", "==", "79", "46", "13"], SingleTestFun("three_part_file.txt"           )),
        ?_assertEqual([                                    "==",       "=="                  ], SingleTestFun("three_part_empty_file.txt"     )),

        ?_assertEqual([[                   "Third", "Second", "First"], [4, 3, 2, 1]], F2TestFun("two_part_file.txt"             )),
        ?_assertEqual([[],                                                    [2, 1]], F2TestFun("two_part_first_empty_file.txt" )),
        ?_assertEqual([["Fifth", "Fourth", "Third", "Second", "First"],           []], F2TestFun("two_part_second_empty_file.txt")),
        ?_assertEqual([[],                                                        []], F2TestFun("two_part_empty_file.txt"       )),

        ?_assertEqual([[78, 45, 12], [13], [80, 79, 68, 57, 46, 35]], F3TestFun("three_part_file.txt"      )),
        ?_assertEqual([[],           [],   []                      ], F3TestFun("three_part_empty_file.txt"))
    ].


read_lines_no_new_line_to_elems_test_() ->
    SingleFun = fun([First|Line]) -> [First, lists:last(Line)] end,
    SingleTestFun = fun(File) -> ja_erl_utils_file:read_lines_no_new_line_to_elems(path(File), SingleFun) end,
    F2Part1Fun = fun(Line) -> lists:append(lists:map(fun(Word) -> erlang:integer_to_list(string:length(Word)) end, string:split(Line, " ", all))) end,
    F2Part2Fun = fun(Line) -> [Index, "line", Part, "part"] = string:split(Line, " ", all), erlang:list_to_integer(Part ++ Index) end,
    F2Separator = "----\n",
    F2TestFun = fun(File) -> ja_erl_utils_file:read_lines_no_new_line_to_elems(path(File), [F2Part1Fun, F2Part2Fun], F2Separator) end,
    F3Part1Fun = fun(IntStr) -> erlang:list_to_integer(IntStr) + 1 end,
    F3Part2Fun = fun(IntStr) -> erlang:list_to_integer(IntStr) * 2 end,
    F3Part3Fun = fun(IntStr) -> erlang:list_to_integer(IntStr) - 1 end,
    F3Separator = "===\n",
    F3TestFun = fun(File) -> ja_erl_utils_file:read_lines_no_new_line_to_elems(path(File), [F3Part1Fun, F3Part2Fun, F3Part3Fun], F3Separator) end,
    [
        ?_assertEqual(["Se"],                                                                   SingleTestFun("single_line.txt"               )),
        ?_assertEqual(["4t", "3t", "2t", "1t", "--",             "Tt", "St", "Ft"],             SingleTestFun("two_part_file.txt"             )),
        ?_assertEqual([            "2t", "1t", "--"                              ],             SingleTestFun("two_part_first_empty_file.txt" )),
        ?_assertEqual([                        "--", "Ft", "Ft", "Tt", "St", "Ft"],             SingleTestFun("two_part_second_empty_file.txt")),
        ?_assertEqual([                        "--"                              ],             SingleTestFun("two_part_empty_file.txt"       )),
        ?_assertEqual(["60", "59", "48", "37", "26", "15", "==", "31", "==", "79", "46", "13"], SingleTestFun("three_part_file.txt"           )),
        ?_assertEqual([                                    "==",       "=="                  ], SingleTestFun("three_part_empty_file.txt"     )),

        ?_assertEqual([[                "5454", "6454", "5454"], [24, 23, 22, 21]], F2TestFun("two_part_file.txt"             )),
        ?_assertEqual([[],                                               [22, 21]], F2TestFun("two_part_first_empty_file.txt" )),
        ?_assertEqual([["5454", "6454", "5454", "6454", "5454"],               []], F2TestFun("two_part_second_empty_file.txt")),
        ?_assertEqual([[],                                                     []], F2TestFun("two_part_empty_file.txt"       )),

        ?_assertEqual([[790, 457, 124], [642], [679, 578, 467, 356, 245, 134]], F3TestFun("three_part_file.txt"      )),
        ?_assertEqual([[],              [],    []                            ], F3TestFun("three_part_empty_file.txt"))
    ].


read_line_groups_test_() ->
    LastFun = fun
        LastFun([Last|"\n"]) -> Last;
        LastFun([_|Line])    -> LastFun(Line)
    end,
    GroupToElemFun = fun(Lines) ->
        lists:append(lists:map(fun([First|Line]) -> [First, LastFun(Line)] end, Lines))
    end,
    F2Separator = "----\n",
    F3Separator = "===\n",
    [
        ?_assertEqual(["Se"],                           ja_erl_utils_file:read_line_groups(path("single_line.txt"               ), GroupToElemFun, undefined  )),
        ?_assertEqual(["Se"],                           ja_erl_utils_file:read_line_groups(path("single_line.txt"               ), GroupToElemFun, F2Separator)),
        ?_assertEqual(["Se"],                           ja_erl_utils_file:read_line_groups(path("single_line.txt"               ), GroupToElemFun, F3Separator)),
        ?_assertEqual(["4t3t2t1t--TtStFt"],             ja_erl_utils_file:read_line_groups(path("two_part_file.txt"             ), GroupToElemFun, undefined  )),
        ?_assertEqual(["4t3t2t1t--TtStFt"],             ja_erl_utils_file:read_line_groups(path("two_part_file.txt"             ), GroupToElemFun, F3Separator)),
        ?_assertEqual(["4t3t2t1t", "TtStFt"],           ja_erl_utils_file:read_line_groups(path("two_part_file.txt"             ), GroupToElemFun, F2Separator)),
        ?_assertEqual(["2t1t", ""],                     ja_erl_utils_file:read_line_groups(path("two_part_first_empty_file.txt" ), GroupToElemFun, F2Separator)),
        ?_assertEqual(["", "FtFtTtStFt"],               ja_erl_utils_file:read_line_groups(path("two_part_second_empty_file.txt"), GroupToElemFun, F2Separator)),
        ?_assertEqual(["", ""],                         ja_erl_utils_file:read_line_groups(path("two_part_empty_file.txt"       ), GroupToElemFun, F2Separator)),
        ?_assertEqual(["605948372615==31==794613"],     ja_erl_utils_file:read_line_groups(path("three_part_file.txt"           ), GroupToElemFun, undefined  )),
        ?_assertEqual(["605948372615==31==794613"],     ja_erl_utils_file:read_line_groups(path("three_part_file.txt"           ), GroupToElemFun, F2Separator)),
        ?_assertEqual(["605948372615", "31", "794613"], ja_erl_utils_file:read_line_groups(path("three_part_file.txt"           ), GroupToElemFun, F3Separator)),
        ?_assertEqual(["", "", ""],                     ja_erl_utils_file:read_line_groups(path("three_part_empty_file.txt"     ), GroupToElemFun, F3Separator))
    ].


read_line_groups_no_new_line_test_() ->
    GroupToElemFun = fun(Lines) ->
        lists:append(lists:map(fun([First|Line]) -> [First, lists:last(Line)] end, Lines))
    end,
    F2Separator = "----\n",
    F3Separator = "===\n",
    [
        ?_assertEqual(["Se"],                           ja_erl_utils_file:read_line_groups_no_new_line(path("single_line.txt"               ), GroupToElemFun, undefined  )),
        ?_assertEqual(["Se"],                           ja_erl_utils_file:read_line_groups_no_new_line(path("single_line.txt"               ), GroupToElemFun, F2Separator)),
        ?_assertEqual(["Se"],                           ja_erl_utils_file:read_line_groups_no_new_line(path("single_line.txt"               ), GroupToElemFun, F3Separator)),
        ?_assertEqual(["4t3t2t1t--TtStFt"],             ja_erl_utils_file:read_line_groups_no_new_line(path("two_part_file.txt"             ), GroupToElemFun, undefined  )),
        ?_assertEqual(["4t3t2t1t--TtStFt"],             ja_erl_utils_file:read_line_groups_no_new_line(path("two_part_file.txt"             ), GroupToElemFun, F3Separator)),
        ?_assertEqual(["4t3t2t1t", "TtStFt"],           ja_erl_utils_file:read_line_groups_no_new_line(path("two_part_file.txt"             ), GroupToElemFun, F2Separator)),
        ?_assertEqual(["2t1t", ""],                     ja_erl_utils_file:read_line_groups_no_new_line(path("two_part_first_empty_file.txt" ), GroupToElemFun, F2Separator)),
        ?_assertEqual(["", "FtFtTtStFt"],               ja_erl_utils_file:read_line_groups_no_new_line(path("two_part_second_empty_file.txt"), GroupToElemFun, F2Separator)),
        ?_assertEqual(["", ""],                         ja_erl_utils_file:read_line_groups_no_new_line(path("two_part_empty_file.txt"       ), GroupToElemFun, F2Separator)),
        ?_assertEqual(["605948372615==31==794613"],     ja_erl_utils_file:read_line_groups_no_new_line(path("three_part_file.txt"           ), GroupToElemFun, undefined  )),
        ?_assertEqual(["605948372615==31==794613"],     ja_erl_utils_file:read_line_groups_no_new_line(path("three_part_file.txt"           ), GroupToElemFun, F2Separator)),
        ?_assertEqual(["605948372615", "31", "794613"], ja_erl_utils_file:read_line_groups_no_new_line(path("three_part_file.txt"           ), GroupToElemFun, F3Separator)),
        ?_assertEqual(["", "", ""],                     ja_erl_utils_file:read_line_groups_no_new_line(path("three_part_empty_file.txt"     ), GroupToElemFun, F3Separator))
    ].


path(FileName) ->
    "test/ja_erl_utils_file_tests/" ++ FileName.