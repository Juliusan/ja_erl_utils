%%%
%%% Unit tests for ja_erl_utils_list.
%%%
-module(ja_erl_utils_list_tests).
-include_lib("eunit/include/eunit.hrl").


count_elems_sorted_test_() ->
    [
        ?_assertEqual({3, [16, 30, 123]}, ja_erl_utils_list:count_elems_sorted(15, [1, 2, 3, 15, 15, 15, 16, 30, 123])),
        ?_assertEqual({0, [16, 30, 123]}, ja_erl_utils_list:count_elems_sorted(15, [1, 2, 3,             16, 30, 123])),
        ?_assertEqual({1, [16, 30, 123]}, ja_erl_utils_list:count_elems_sorted(15, [         15,         16, 30, 123])),
        ?_assertEqual({3, [16, 30, 123]}, ja_erl_utils_list:count_elems_sorted(15, [         15, 15, 15, 16, 30, 123]))
    ].


count_elems_start_test_() ->
    [
        ?_assertEqual({1, [16, 30, 123]}, ja_erl_utils_list:count_elems_start([15,         16, 30, 123])),
        ?_assertEqual({3, [16, 30, 123]}, ja_erl_utils_list:count_elems_start([15, 15, 15, 16, 30, 123]))
    ].


is_strictly_increasing_test_() ->
    [
        ?_assertEqual(true,  ja_erl_utils_list:is_strictly_increasing([]                  )),
        ?_assertEqual(true,  ja_erl_utils_list:is_strictly_increasing([20]                )),
        ?_assertEqual(true,  ja_erl_utils_list:is_strictly_increasing([20, 21, 22, 23, 24])),
        ?_assertEqual(false, ja_erl_utils_list:is_strictly_increasing([20, 21, 22, 23, 19])),
        ?_assertEqual(false, ja_erl_utils_list:is_strictly_increasing([20, 21, 22, 22, 23])),
        ?_assertEqual(false, ja_erl_utils_list:is_strictly_increasing([20, 19, 21, 23, 25])),

        ?_assertEqual(true,  ja_erl_utils_list:is_strictly_increasing([],                   1)),
        ?_assertEqual(true,  ja_erl_utils_list:is_strictly_increasing([20],                 1)),
        ?_assertEqual(true,  ja_erl_utils_list:is_strictly_increasing([20, 21, 22, 23, 24], 1)),
        ?_assertEqual(false, ja_erl_utils_list:is_strictly_increasing([20, 21, 22, 23, 22], 1)),
        ?_assertEqual(false, ja_erl_utils_list:is_strictly_increasing([20, 21, 22, 25, 28], 1)),
        ?_assertEqual(true,  ja_erl_utils_list:is_strictly_increasing([20, 21, 22, 23, 24], 3)),
        ?_assertEqual(false, ja_erl_utils_list:is_strictly_increasing([20, 21, 22, 22, 23], 3)),
        ?_assertEqual(true,  ja_erl_utils_list:is_strictly_increasing([20, 21, 22, 25, 28], 3)),
        ?_assertEqual(false, ja_erl_utils_list:is_strictly_increasing([20, 25, 26, 27, 28], 3)),
        ?_assertEqual(false, ja_erl_utils_list:is_strictly_increasing([20, 19, 21, 23, 25], 3))
    ].


is_increasing_test_() ->
    [
        ?_assertEqual(true,  ja_erl_utils_list:is_increasing([]                  )),
        ?_assertEqual(true,  ja_erl_utils_list:is_increasing([20]                )),
        ?_assertEqual(true,  ja_erl_utils_list:is_increasing([20, 21, 22, 23, 24])),
        ?_assertEqual(false, ja_erl_utils_list:is_increasing([20, 21, 22, 23, 19])),
        ?_assertEqual(true,  ja_erl_utils_list:is_increasing([20, 21, 22, 22, 23])),
        ?_assertEqual(false, ja_erl_utils_list:is_increasing([20, 19, 21, 23, 25])),

        ?_assertEqual(true,  ja_erl_utils_list:is_increasing([],                   1)),
        ?_assertEqual(true,  ja_erl_utils_list:is_increasing([20],                 1)),
        ?_assertEqual(true,  ja_erl_utils_list:is_increasing([20, 21, 22, 23, 24], 1)),
        ?_assertEqual(false, ja_erl_utils_list:is_increasing([20, 21, 22, 23, 22], 1)),
        ?_assertEqual(false, ja_erl_utils_list:is_increasing([20, 21, 22, 25, 28], 1)),
        ?_assertEqual(true,  ja_erl_utils_list:is_increasing([20, 21, 22, 23, 24], 3)),
        ?_assertEqual(true,  ja_erl_utils_list:is_increasing([20, 21, 22, 22, 23], 3)),
        ?_assertEqual(true,  ja_erl_utils_list:is_increasing([20, 21, 22, 25, 28], 3)),
        ?_assertEqual(false, ja_erl_utils_list:is_increasing([20, 25, 26, 27, 28], 3)),
        ?_assertEqual(false, ja_erl_utils_list:is_increasing([20, 19, 21, 23, 25], 3))
    ].


is_strictly_decreasing_test_() ->
    [
        ?_assertEqual(true,  ja_erl_utils_list:is_strictly_decreasing([]                  )),
        ?_assertEqual(true,  ja_erl_utils_list:is_strictly_decreasing([20]                )),
        ?_assertEqual(true,  ja_erl_utils_list:is_strictly_decreasing([20, 19, 18, 17, 16])),
        ?_assertEqual(false, ja_erl_utils_list:is_strictly_decreasing([20, 19, 18, 17, 21])),
        ?_assertEqual(false, ja_erl_utils_list:is_strictly_decreasing([20, 19, 18, 18, 17])),
        ?_assertEqual(false, ja_erl_utils_list:is_strictly_decreasing([20, 21, 19, 17, 15])),

        ?_assertEqual(true,  ja_erl_utils_list:is_strictly_decreasing([],                   1)),
        ?_assertEqual(true,  ja_erl_utils_list:is_strictly_decreasing([20],                 1)),
        ?_assertEqual(true,  ja_erl_utils_list:is_strictly_decreasing([20, 19, 18, 17, 16], 1)),
        ?_assertEqual(false, ja_erl_utils_list:is_strictly_decreasing([20, 19, 18, 17, 21], 1)),
        ?_assertEqual(false, ja_erl_utils_list:is_strictly_decreasing([20, 19, 18, 15, 12], 1)),
        ?_assertEqual(true,  ja_erl_utils_list:is_strictly_decreasing([20, 19, 18, 17, 16], 3)),
        ?_assertEqual(false, ja_erl_utils_list:is_strictly_decreasing([20, 19, 18, 18, 17], 3)),
        ?_assertEqual(true,  ja_erl_utils_list:is_strictly_decreasing([20, 19, 18, 15, 12], 3)),
        ?_assertEqual(false, ja_erl_utils_list:is_strictly_decreasing([20, 15, 14, 13, 12], 3)),
        ?_assertEqual(false, ja_erl_utils_list:is_strictly_decreasing([20, 21, 19, 17, 15], 3))
    ].


is_decreasing_test_() ->
    [
        ?_assertEqual(true,  ja_erl_utils_list:is_decreasing([]                  )),
        ?_assertEqual(true,  ja_erl_utils_list:is_decreasing([20]                )),
        ?_assertEqual(true,  ja_erl_utils_list:is_decreasing([20, 19, 18, 17, 16])),
        ?_assertEqual(false, ja_erl_utils_list:is_decreasing([20, 19, 18, 17, 21])),
        ?_assertEqual(true,  ja_erl_utils_list:is_decreasing([20, 19, 18, 18, 17])),
        ?_assertEqual(false, ja_erl_utils_list:is_decreasing([20, 21, 19, 17, 15])),

        ?_assertEqual(true,  ja_erl_utils_list:is_decreasing([],                   1)),
        ?_assertEqual(true,  ja_erl_utils_list:is_decreasing([20],                 1)),
        ?_assertEqual(true,  ja_erl_utils_list:is_decreasing([20, 19, 18, 17, 16], 1)),
        ?_assertEqual(false, ja_erl_utils_list:is_decreasing([20, 19, 18, 17, 21], 1)),
        ?_assertEqual(false, ja_erl_utils_list:is_decreasing([20, 19, 18, 15, 12], 1)),
        ?_assertEqual(true,  ja_erl_utils_list:is_decreasing([20, 19, 18, 17, 16], 3)),
        ?_assertEqual(true,  ja_erl_utils_list:is_decreasing([20, 19, 18, 18, 17], 3)),
        ?_assertEqual(true,  ja_erl_utils_list:is_decreasing([20, 19, 18, 15, 12], 3)),
        ?_assertEqual(false, ja_erl_utils_list:is_decreasing([20, 15, 14, 13, 12], 3)),
        ?_assertEqual(false, ja_erl_utils_list:is_decreasing([20, 21, 19, 17, 15], 3))
    ].


is_steady_test_() ->
    [
        ?_assertEqual(true,  ja_erl_utils_list:is_steady([],                   1)),
        ?_assertEqual(true,  ja_erl_utils_list:is_steady([20],                 1)),
        ?_assertEqual(true,  ja_erl_utils_list:is_steady([20, 21, 22, 23, 24], 1)),
        ?_assertEqual(true,  ja_erl_utils_list:is_steady([20, 19, 18, 17, 16], 1)),
        ?_assertEqual(true,  ja_erl_utils_list:is_steady([20, 19, 20, 21, 20], 1)),
        ?_assertEqual(true,  ja_erl_utils_list:is_steady([20, 19, 19, 20, 21], 1)),
        ?_assertEqual(false, ja_erl_utils_list:is_steady([20, 19, 21, 20, 19], 1)),
        ?_assertEqual(false, ja_erl_utils_list:is_steady([20, 18, 19, 20, 19], 1)),
        ?_assertEqual(true,  ja_erl_utils_list:is_steady([20, 21, 23, 26, 27], 3)),
        ?_assertEqual(true,  ja_erl_utils_list:is_steady([20, 19, 17, 14, 13], 3)),
        ?_assertEqual(true,  ja_erl_utils_list:is_steady([20, 21, 19, 22, 21], 3)),
        ?_assertEqual(true,  ja_erl_utils_list:is_steady([20, 21, 21, 19, 22], 3)),
        ?_assertEqual(false, ja_erl_utils_list:is_steady([20, 21, 19, 23, 22], 3)),
        ?_assertEqual(false, ja_erl_utils_list:is_steady([20, 21, 19, 22, 18], 3))
    ].


is_monotonic_test_() ->
    Mul3Fun = fun(Elem1, Elem2) -> Elem2 =:= 3*Elem1 end,
    MulRangeFun = fun(Elem1, Elem2) -> abs(Elem2) < abs(3*Elem1) end,
    [
        ?_assertEqual(true,  ja_erl_utils_list:is_monotonic([],                  Mul3Fun    )),
        ?_assertEqual(true,  ja_erl_utils_list:is_monotonic([1],                 Mul3Fun    )),
        ?_assertEqual(true,  ja_erl_utils_list:is_monotonic([1, 3, 9, 27, 81],   Mul3Fun    )),
        ?_assertEqual(false, ja_erl_utils_list:is_monotonic([1, 3, 9, 26, 78],   Mul3Fun    )),
        ?_assertEqual(true,  ja_erl_utils_list:is_monotonic([],                  MulRangeFun)),
        ?_assertEqual(true,  ja_erl_utils_list:is_monotonic([1],                 MulRangeFun)),
        ?_assertEqual(true,  ja_erl_utils_list:is_monotonic([1, -2, -5, 10, 11], MulRangeFun)),
        ?_assertEqual(false, ja_erl_utils_list:is_monotonic([1, -3, -5, 10, 11], MulRangeFun))
    ].


transpose_test_() ->
    [
        ?_assertEqual([],                        ja_erl_utils_list:transpose([])),
        ?_assertEqual([[1]  ],                   ja_erl_utils_list:transpose([[1]  ])),
        ?_assertEqual([[1],    [2]    ],         ja_erl_utils_list:transpose([[1,2]])),
        ?_assertEqual([[1,2]],                   ja_erl_utils_list:transpose([[1],    [2]    ])),
        ?_assertEqual([[1,4],  [2,5],  [3,6]  ], ja_erl_utils_list:transpose([[1,2,3],[4,5,6]])),
        ?_assertEqual([[1,2,3],[4,5,6]],         ja_erl_utils_list:transpose([[1,4],  [2,5],  [3,6]  ])),
        ?_assertEqual([[1,4,7],[2,5,8],[3,6,9]], ja_erl_utils_list:transpose([[1,2,3],[4,5,6],[7,8,9]]))
    ].


diagonals_f_test_() ->
    [
        ?_assertEqual([],                            ja_erl_utils_list:diagonals_f([])),
        ?_assertEqual([[1]],                         ja_erl_utils_list:diagonals_f([[1]  ])),
        ?_assertEqual([[2],[1]],                     ja_erl_utils_list:diagonals_f([[1,2]])),
        ?_assertEqual([[2],[1]],                     ja_erl_utils_list:diagonals_f([[1],    [2]    ])),
        ?_assertEqual([[4],[3,2],[1]],               ja_erl_utils_list:diagonals_f([[1,2],  [3,4]  ])),
        ?_assertEqual([[6],[5,3],[4,2],[1]],         ja_erl_utils_list:diagonals_f([[1,2,3],[4,5,6]])),
        ?_assertEqual([[6],[5,4],[3,2],[1]],         ja_erl_utils_list:diagonals_f([[1,2],  [3,4],  [5,6]  ])),
        ?_assertEqual([[9],[8,6],[7,5,3],[4,2],[1]], ja_erl_utils_list:diagonals_f([[1,2,3],[4,5,6],[7,8,9]]))
    ].


diagonals_b_test_() ->
    [
        ?_assertEqual([],                            ja_erl_utils_list:diagonals_b([])),
        ?_assertEqual([[1]],                         ja_erl_utils_list:diagonals_b([[1]  ])),
        ?_assertEqual([[1],[2]],                     ja_erl_utils_list:diagonals_b([[1,2]])),
        ?_assertEqual([[2],[1]],                     ja_erl_utils_list:diagonals_b([[1],    [2]    ])),
        ?_assertEqual([[3],[4,1],[2]],               ja_erl_utils_list:diagonals_b([[1,2],  [3,4]  ])),
        ?_assertEqual([[4],[5,1],[6,2],[3]],         ja_erl_utils_list:diagonals_b([[1,2,3],[4,5,6]])),
        ?_assertEqual([[5],[6,3],[4,1],[2]],         ja_erl_utils_list:diagonals_b([[1,2],  [3,4],  [5,6]  ])),
        ?_assertEqual([[7],[8,4],[9,5,1],[6,2],[3]], ja_erl_utils_list:diagonals_b([[1,2,3],[4,5,6],[7,8,9]]))
    ].


middle_test_() ->
    [
        ?_assertEqual([1],   ja_erl_utils_list:middle([1]          )),
        ?_assertEqual([1,2], ja_erl_utils_list:middle([1,2]        )),
        ?_assertEqual([2],   ja_erl_utils_list:middle([1,2,3]      )),
        ?_assertEqual([2,3], ja_erl_utils_list:middle([1,2,3,4]    )),
        ?_assertEqual([n],   ja_erl_utils_list:middle([a,6,n,u,9]  )),
        ?_assertEqual([n,5], ja_erl_utils_list:middle([a,6,n,5,u,9]))
    ].


middle_single_test_() ->
    [
        ?_assertEqual(1, ja_erl_utils_list:middle_single([1]          )),
        ?_assertEqual(2, ja_erl_utils_list:middle_single([1,2,3]      )),
        ?_assertEqual(n, ja_erl_utils_list:middle_single([a,6,n,u,9]  )),
        ?_assertError(_, ja_erl_utils_list:middle_single([1,2]        )),
        ?_assertError(_, ja_erl_utils_list:middle_single([1,2,3,4]    )),
        ?_assertError(_, ja_erl_utils_list:middle_single([a,6,n,5,u,9]))
    ].


intersections_test_() ->
    [
        ?_assertEqual([],    ja_erl_utils_list:intersection([     ], [     ])),
        ?_assertEqual([],    ja_erl_utils_list:intersection([1,2,3], [4,5,6])),
        ?_assertEqual([2,3], ja_erl_utils_list:intersection([1,2,3], [2,3,4])),
        ?_assertEqual([2,3], ja_erl_utils_list:intersection([1,2,3], [2,3  ])),
        ?_assertEqual([2,3], ja_erl_utils_list:intersection([  2,3], [2,3,4])),
        ?_assertEqual([2,3], ja_erl_utils_list:intersection([1,2,3], [3,4,2])),
        ?_assertEqual([2,3], ja_erl_utils_list:intersection([1,2,3], [3,  2])),
        ?_assertEqual([2,3], ja_erl_utils_list:intersection([  2,3], [3,4,2]))
    ].


foldl_pairs_test_() ->
    [
        ?_assertEqual(ok,                                    ja_erl_utils_list:foldl_pairs(fun(_,  _,  _) -> fail end,         ok, [       ])),
        ?_assertEqual(ok,                                    ja_erl_utils_list:foldl_pairs(fun(_,  _,  _) -> fail end,         ok, [1      ])),
        ?_assertEqual([                              {1,2}], ja_erl_utils_list:foldl_pairs(fun(E1, E2, A) -> [{E1, E2}|A] end, [], [1,2    ])),
        ?_assertEqual([            {2,3},      {1,3},{1,2}], ja_erl_utils_list:foldl_pairs(fun(E1, E2, A) -> [{E1, E2}|A] end, [], [1,2,3  ])),
        ?_assertEqual([{3,4},{2,4},{2,3},{1,4},{1,3},{1,2}], ja_erl_utils_list:foldl_pairs(fun(E1, E2, A) -> [{E1, E2}|A] end, [], [1,2,3,4]))
    ].


map_sum_test_() ->
    [
        ?_assertEqual(0,  ja_erl_utils_list:map_sum(fun(A) -> A-$0 end, [              ])),
        ?_assertEqual(1,  ja_erl_utils_list:map_sum(fun(A) -> A-$0 end, [$1            ])),
        ?_assertEqual(10, ja_erl_utils_list:map_sum(fun(A) -> A-$0 end, [$1, $2, $3, $4]))
    ].


map_sum_foldl_test_() ->
    [
        ?_assertEqual({0, 10}, ja_erl_utils_list:map_sum_foldl(fun(A, B) -> {B*(A-$0), B-1} end, 10, [              ])),
        ?_assertEqual({10, 9}, ja_erl_utils_list:map_sum_foldl(fun(A, B) -> {B*(A-$0), B-1} end, 10, [$1            ])),
        ?_assertEqual({80, 6}, ja_erl_utils_list:map_sum_foldl(fun(A, B) -> {B*(A-$0), B-1} end, 10, [$1, $2, $3, $4]))
    ].


filter_count_test_() ->
    [
        ?_assertEqual(0, ja_erl_utils_list:filter_count(fun(_) -> true               end, [                  ])),
        ?_assertEqual(0, ja_erl_utils_list:filter_count(fun(A) -> (A-$0) rem 2 =:= 0 end, [$1                ])),
        ?_assertEqual(1, ja_erl_utils_list:filter_count(fun(A) -> (A-$0) rem 2 =:= 0 end, [$2                ])),
        ?_assertEqual(2, ja_erl_utils_list:filter_count(fun(A) -> (A-$0) rem 2 =:= 0 end, [$1, $2, $3, $4, $5]))
    ].


filter_count_foldl_test_() ->
    [
        ?_assertEqual({0, 10}, ja_erl_utils_list:filter_count_foldl(fun(A, B) -> {B*(A-$0) rem 2 =:= 0, B-1} end, 10, [              ])),
        ?_assertEqual({0, 10}, ja_erl_utils_list:filter_count_foldl(fun(A, B) -> {B*(A-$0) rem 2 =:= 0, B-1} end, 11, [$1            ])),
        ?_assertEqual({1,  9}, ja_erl_utils_list:filter_count_foldl(fun(A, B) -> {B*(A-$0) rem 2 =:= 0, B-1} end, 10, [$2            ])),
        ?_assertEqual({3,  6}, ja_erl_utils_list:filter_count_foldl(fun(A, B) -> {B*(A-$0) rem 2 =:= 0, B-1} end, 10, [$1, $2, $4, $5]))
    ].
