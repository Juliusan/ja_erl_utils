%%%
%%% Unit tests for ja_erl_utils_matrix.
%%%
-module(ja_erl_utils_matrix_tests).
-include_lib("eunit/include/eunit.hrl").


new_test_() ->
    [
        ?'_assertEqual'({matrix, 2, 3, undefined, #{}}, ja_erl_utils_matrix:new(2, 3)),
        ?'_assertEqual'({matrix, 3, 4, "default", #{}}, ja_erl_utils_matrix:new(3, 4, "default")),

        ?'_assertError'(badarg, ja_erl_utils_matrix:new(-2,  3)),
        ?'_assertError'(badarg, ja_erl_utils_matrix:new( 0,  3)),
        ?'_assertError'(badarg, ja_erl_utils_matrix:new( 2, -3)),
        ?'_assertError'(badarg, ja_erl_utils_matrix:new( 2,  0)),
        ?'_assertError'(badarg, ja_erl_utils_matrix:new(-2, -3)),
        ?'_assertError'(badarg, ja_erl_utils_matrix:new(-2, -3, "default"))
    ].


get_test_() ->
    Matrix = {matrix, 2, 3, x, #{{1, 1}=>a, {1,2}=>b, {1,3}=>c, {2,1}=> d}},
    [
        ?'_assertEqual'(a, ja_erl_utils_matrix:get({1, 1}, Matrix)),
        ?'_assertEqual'(b, ja_erl_utils_matrix:get({1, 2}, Matrix)),
        ?'_assertEqual'(c, ja_erl_utils_matrix:get({1, 3}, Matrix)),
        ?'_assertEqual'(d, ja_erl_utils_matrix:get({2, 1}, Matrix)),
        ?'_assertEqual'(x, ja_erl_utils_matrix:get({2, 2}, Matrix)),
        ?'_assertEqual'(x, ja_erl_utils_matrix:get({2, 3}, Matrix)),

        ?'_assertError'(badarg, ja_erl_utils_matrix:get({-2,  3}, Matrix)),
        ?'_assertError'(badarg, ja_erl_utils_matrix:get({ 0,  3}, Matrix)),
        ?'_assertError'(badarg, ja_erl_utils_matrix:get({ 3,  3}, Matrix)),
        ?'_assertError'(badarg, ja_erl_utils_matrix:get({ 2, -3}, Matrix)),
        ?'_assertError'(badarg, ja_erl_utils_matrix:get({ 2,  0}, Matrix)),
        ?'_assertError'(badarg, ja_erl_utils_matrix:get({ 2,  4}, Matrix)),
        ?'_assertError'(badarg, ja_erl_utils_matrix:get({-2,  4}, Matrix))
    ].


set_test_() ->
    Values = #{{1, 1}=>a, {1,2}=>b, {1,3}=>c, {2,1}=> d},
    Matrix = {matrix, 2, 3, x, Values},
    [
        ?'_assertEqual'(erlang:setelement(5, Matrix, Values#{{1, 1} => 100}), ja_erl_utils_matrix:set({1, 1}, Matrix, 100)),
        ?'_assertEqual'(erlang:setelement(5, Matrix, Values#{{1, 2} => 100}), ja_erl_utils_matrix:set({1, 2}, Matrix, 100)),
        ?'_assertEqual'(erlang:setelement(5, Matrix, Values#{{1, 3} => 100}), ja_erl_utils_matrix:set({1, 3}, Matrix, 100)),
        ?'_assertEqual'(erlang:setelement(5, Matrix, Values#{{2, 1} => 100}), ja_erl_utils_matrix:set({2, 1}, Matrix, 100)),
        ?'_assertEqual'(erlang:setelement(5, Matrix, Values#{{2, 2} => 100}), ja_erl_utils_matrix:set({2, 2}, Matrix, 100)),
        ?'_assertEqual'(erlang:setelement(5, Matrix, Values#{{2, 3} => 100}), ja_erl_utils_matrix:set({2, 3}, Matrix, 100)),

        ?'_assertError'(badarg, ja_erl_utils_matrix:set({-2,  3}, Matrix, 100)),
        ?'_assertError'(badarg, ja_erl_utils_matrix:set({ 0,  3}, Matrix, 100)),
        ?'_assertError'(badarg, ja_erl_utils_matrix:set({ 3,  3}, Matrix, 100)),
        ?'_assertError'(badarg, ja_erl_utils_matrix:set({ 2, -3}, Matrix, 100)),
        ?'_assertError'(badarg, ja_erl_utils_matrix:set({ 2,  0}, Matrix, 100)),
        ?'_assertError'(badarg, ja_erl_utils_matrix:set({ 2,  4}, Matrix, 100)),
        ?'_assertError'(badarg, ja_erl_utils_matrix:set({-2,  4}, Matrix, 100))
    ].


is_valid_index_test_() ->
    Matrix = {matrix, 8, 8, undefined, #{}},
    [
        ?_assertEqual(true,  ja_erl_utils_matrix:is_valid_index({2,3}, Matrix)),
        ?_assertEqual(true,  ja_erl_utils_matrix:is_valid_index({1,1}, Matrix)),
        ?_assertEqual(true,  ja_erl_utils_matrix:is_valid_index({1,5}, Matrix)),
        ?_assertEqual(true,  ja_erl_utils_matrix:is_valid_index({1,8}, Matrix)),
        ?_assertEqual(true,  ja_erl_utils_matrix:is_valid_index({5,8}, Matrix)),
        ?_assertEqual(true,  ja_erl_utils_matrix:is_valid_index({8,8}, Matrix)),
        ?_assertEqual(true,  ja_erl_utils_matrix:is_valid_index({8,5}, Matrix)),
        ?_assertEqual(true,  ja_erl_utils_matrix:is_valid_index({8,1}, Matrix)),
        ?_assertEqual(true,  ja_erl_utils_matrix:is_valid_index({5,1}, Matrix)),
        ?_assertEqual(false, ja_erl_utils_matrix:is_valid_index({0,0}, Matrix)),
        ?_assertEqual(false, ja_erl_utils_matrix:is_valid_index({0,5}, Matrix)),
        ?_assertEqual(false, ja_erl_utils_matrix:is_valid_index({0,9}, Matrix)),
        ?_assertEqual(false, ja_erl_utils_matrix:is_valid_index({5,9}, Matrix)),
        ?_assertEqual(false, ja_erl_utils_matrix:is_valid_index({9,9}, Matrix)),
        ?_assertEqual(false, ja_erl_utils_matrix:is_valid_index({9,5}, Matrix)),
        ?_assertEqual(false, ja_erl_utils_matrix:is_valid_index({9,0}, Matrix)),
        ?_assertEqual(false, ja_erl_utils_matrix:is_valid_index({5,0}, Matrix))
    ].


values_test_() ->
    Matrix1 = {matrix, 1, 1, a, #{        }},
    Matrix2 = {matrix, 1, 1, a, #{{1,1}=>5}},
    Matrix3 = {matrix, 2, 3, a, #{{1,1}=>1,         {1,3}=>3,         {2,2}=>5,{2,3}=>6}},
    Matrix4 = {matrix, 2, 3, a, #{{1,1}=>1,{1,2}=>2,{1,3}=>3,{2,1}=>4,{2,2}=>5,{2,3}=>6}},
    [
        ?_assertEqual([a],                ja_erl_utils_matrix:values(Matrix1)),
        ?_assertEqual([5],                ja_erl_utils_matrix:values(Matrix2)),
        ?_assertEqual([1, a, 3, a, 5, 6], ja_erl_utils_matrix:values(Matrix3)),
        ?_assertEqual([1, 2, 3, 4, 5, 6], ja_erl_utils_matrix:values(Matrix4))
    ].


index_of_test_() ->
    Matrix = {matrix, 2, 3, x, #{{1,1}=>a,{1,2}=>b,{1,3}=>c,{2,2}=>d,{2,3}=>e}},
    [
        ?_assertEqual({1,1},     ja_erl_utils_matrix:index_of(a, Matrix)),
        ?_assertEqual({1,2},     ja_erl_utils_matrix:index_of(b, Matrix)),
        ?_assertEqual({1,3},     ja_erl_utils_matrix:index_of(c, Matrix)),
        ?_assertEqual({2,1},     ja_erl_utils_matrix:index_of(x, Matrix)),
        ?_assertEqual({2,2},     ja_erl_utils_matrix:index_of(d, Matrix)),
        ?_assertEqual({2,3},     ja_erl_utils_matrix:index_of(e, Matrix)),
        ?_assertEqual(undefined, ja_erl_utils_matrix:index_of(f, Matrix))
    ].


next_index_test_() ->
    Matrix = {matrix, 8, 8, undefined, #{}},
    [
        ?_assertEqual({1,3},     ja_erl_utils_matrix:next_index({2,3}, up,    Matrix)),
        ?_assertEqual({2,4},     ja_erl_utils_matrix:next_index({2,3}, right, Matrix)),
        ?_assertEqual({3,3},     ja_erl_utils_matrix:next_index({2,3}, down,  Matrix)),
        ?_assertEqual({2,2},     ja_erl_utils_matrix:next_index({2,3}, left,  Matrix)),
        ?_assertEqual(undefined, ja_erl_utils_matrix:next_index({1,1}, up,    Matrix)),
        ?_assertEqual({1,2},     ja_erl_utils_matrix:next_index({1,1}, right, Matrix)),
        ?_assertEqual({2,1},     ja_erl_utils_matrix:next_index({1,1}, down,  Matrix)),
        ?_assertEqual(undefined, ja_erl_utils_matrix:next_index({1,1}, left,  Matrix)),
        ?_assertEqual(undefined, ja_erl_utils_matrix:next_index({1,5}, up,    Matrix)),
        ?_assertEqual({1,6},     ja_erl_utils_matrix:next_index({1,5}, right, Matrix)),
        ?_assertEqual({2,5},     ja_erl_utils_matrix:next_index({1,5}, down,  Matrix)),
        ?_assertEqual({1,4},     ja_erl_utils_matrix:next_index({1,5}, left,  Matrix)),
        ?_assertEqual(undefined, ja_erl_utils_matrix:next_index({1,8}, up,    Matrix)),
        ?_assertEqual(undefined, ja_erl_utils_matrix:next_index({1,8}, right, Matrix)),
        ?_assertEqual({2,8},     ja_erl_utils_matrix:next_index({1,8}, down,  Matrix)),
        ?_assertEqual({1,7},     ja_erl_utils_matrix:next_index({1,8}, left,  Matrix)),
        ?_assertEqual({4,8},     ja_erl_utils_matrix:next_index({5,8}, up,    Matrix)),
        ?_assertEqual(undefined, ja_erl_utils_matrix:next_index({5,8}, right, Matrix)),
        ?_assertEqual({6,8},     ja_erl_utils_matrix:next_index({5,8}, down,  Matrix)),
        ?_assertEqual({5,7},     ja_erl_utils_matrix:next_index({5,8}, left,  Matrix)),
        ?_assertEqual({7,8},     ja_erl_utils_matrix:next_index({8,8}, up,    Matrix)),
        ?_assertEqual(undefined, ja_erl_utils_matrix:next_index({8,8}, right, Matrix)),
        ?_assertEqual(undefined, ja_erl_utils_matrix:next_index({8,8}, down,  Matrix)),
        ?_assertEqual({8,7},     ja_erl_utils_matrix:next_index({8,8}, left,  Matrix)),
        ?_assertEqual({7,5},     ja_erl_utils_matrix:next_index({8,5}, up,    Matrix)),
        ?_assertEqual({8,6},     ja_erl_utils_matrix:next_index({8,5}, right, Matrix)),
        ?_assertEqual(undefined, ja_erl_utils_matrix:next_index({8,5}, down,  Matrix)),
        ?_assertEqual({8,4},     ja_erl_utils_matrix:next_index({8,5}, left,  Matrix)),
        ?_assertEqual({7,1},     ja_erl_utils_matrix:next_index({8,1}, up,    Matrix)),
        ?_assertEqual({8,2},     ja_erl_utils_matrix:next_index({8,1}, right, Matrix)),
        ?_assertEqual(undefined, ja_erl_utils_matrix:next_index({8,1}, down,  Matrix)),
        ?_assertEqual(undefined, ja_erl_utils_matrix:next_index({8,1}, left,  Matrix)),
        ?_assertEqual({4,1},     ja_erl_utils_matrix:next_index({5,1}, up,    Matrix)),
        ?_assertEqual({5,2},     ja_erl_utils_matrix:next_index({5,1}, right, Matrix)),
        ?_assertEqual({6,1},     ja_erl_utils_matrix:next_index({5,1}, down,  Matrix)),
        ?_assertEqual(undefined, ja_erl_utils_matrix:next_index({5,1}, left,  Matrix))
    ].


get_char_matrix_test_() ->
    [
        ?_assertEqual({matrix, 1, 1, undefined, #{{1,1}=>$a}},                     ja_erl_utils_matrix:get_char_matrix(["a"])),
        ?_assertEqual({matrix, 1, 3, undefined, #{{1,1}=>$a,{1,2}=>$b,{1,3}=>$c}}, ja_erl_utils_matrix:get_char_matrix(["abc"])),
        ?_assertEqual({matrix, 2, 1, undefined, #{{1,1}=>$a,{2,1}=>$b}},           ja_erl_utils_matrix:get_char_matrix(["a", "b"])),
        ?_assertEqual({matrix, 2, 3, undefined, #{{1,1}=>$a,{1,2}=>$b,{1,3}=>$c,
                                                  {2,1}=>$d,{2,2}=>$e,{2,3}=>$f}}, ja_erl_utils_matrix:get_char_matrix(["abc","def"])),
        ?_assertEqual({matrix, 3, 3, undefined, #{{1,1}=>$a,{1,2}=>$b,{1,3}=>$c,
                                                  {2,1}=>$d,{2,2}=>$e,{2,3}=>$f,
                                                  {3,1}=>$g,{3,2}=>$h,{3,3}=>$i}}, ja_erl_utils_matrix:get_char_matrix(["abc","def","ghi"]))
    ].


iterator_next_test_() ->
    Matrix = {matrix, 4, 2, undefined, #{{1,1}=>1, {1,2}=>2,
                                         {2,1}=>3, {2,2}=>4,
                                         {3,1}=>5, {3,2}=>6,
                                         {4,1}=>7, {4,2}=>8}},
    ElementFun = fun
        ElementFun(1, Iterator) ->
            {Index, Value, _} = ja_erl_utils_matrix:next(Iterator),
            {Index, Value};
        ElementFun(N, Iterator) ->
            {_, _, NextIterator} = ja_erl_utils_matrix:next(Iterator),
            ElementFun(N-1, NextIterator)
    end,
    [
        ?_assertEqual({{3, 1}, 5}, ElementFun(5, ja_erl_utils_matrix:iterator(Matrix))),

        ?_assertEqual({{3, 1}, 5}, ElementFun(5, ja_erl_utils_matrix:iterator(Matrix, #{                                                                 }))),
        ?_assertEqual({{3, 1}, 5}, ElementFun(5, ja_erl_utils_matrix:iterator(Matrix, #{row_order => left                                                }))),
        ?_assertEqual({{3, 2}, 6}, ElementFun(5, ja_erl_utils_matrix:iterator(Matrix, #{row_order => right                                               }))),
        ?_assertEqual({{3, 1}, 5}, ElementFun(5, ja_erl_utils_matrix:iterator(Matrix, #{                    column_order => top                          }))),
        ?_assertEqual({{3, 1}, 5}, ElementFun(5, ja_erl_utils_matrix:iterator(Matrix, #{row_order => left,  column_order => top                          }))),
        ?_assertEqual({{3, 2}, 6}, ElementFun(5, ja_erl_utils_matrix:iterator(Matrix, #{row_order => right, column_order => top                          }))),
        ?_assertEqual({{2, 1}, 3}, ElementFun(5, ja_erl_utils_matrix:iterator(Matrix, #{                    column_order => bottom                       }))),
        ?_assertEqual({{2, 1}, 3}, ElementFun(5, ja_erl_utils_matrix:iterator(Matrix, #{row_order => left,  column_order => bottom                       }))),
        ?_assertEqual({{2, 2}, 4}, ElementFun(5, ja_erl_utils_matrix:iterator(Matrix, #{row_order => right, column_order => bottom                       }))),
        ?_assertEqual({{3, 1}, 5}, ElementFun(5, ja_erl_utils_matrix:iterator(Matrix, #{                                            loop_order => rows   }))),
        ?_assertEqual({{3, 1}, 5}, ElementFun(5, ja_erl_utils_matrix:iterator(Matrix, #{row_order => left,                          loop_order => rows   }))),
        ?_assertEqual({{3, 2}, 6}, ElementFun(5, ja_erl_utils_matrix:iterator(Matrix, #{row_order => right,                         loop_order => rows   }))),
        ?_assertEqual({{3, 1}, 5}, ElementFun(5, ja_erl_utils_matrix:iterator(Matrix, #{                    column_order => top,    loop_order => rows   }))),
        ?_assertEqual({{3, 1}, 5}, ElementFun(5, ja_erl_utils_matrix:iterator(Matrix, #{row_order => left,  column_order => top,    loop_order => rows   }))),
        ?_assertEqual({{3, 2}, 6}, ElementFun(5, ja_erl_utils_matrix:iterator(Matrix, #{row_order => right, column_order => top,    loop_order => rows   }))),
        ?_assertEqual({{2, 1}, 3}, ElementFun(5, ja_erl_utils_matrix:iterator(Matrix, #{                    column_order => bottom, loop_order => rows   }))),
        ?_assertEqual({{2, 1}, 3}, ElementFun(5, ja_erl_utils_matrix:iterator(Matrix, #{row_order => left,  column_order => bottom, loop_order => rows   }))),
        ?_assertEqual({{2, 2}, 4}, ElementFun(5, ja_erl_utils_matrix:iterator(Matrix, #{row_order => right, column_order => bottom, loop_order => rows   }))),
        ?_assertEqual({{1, 2}, 2}, ElementFun(5, ja_erl_utils_matrix:iterator(Matrix, #{                                            loop_order => columns}))),
        ?_assertEqual({{1, 2}, 2}, ElementFun(5, ja_erl_utils_matrix:iterator(Matrix, #{row_order => left,                          loop_order => columns}))),
        ?_assertEqual({{1, 1}, 1}, ElementFun(5, ja_erl_utils_matrix:iterator(Matrix, #{row_order => right,                         loop_order => columns}))),
        ?_assertEqual({{1, 2}, 2}, ElementFun(5, ja_erl_utils_matrix:iterator(Matrix, #{                    column_order => top,    loop_order => columns}))),
        ?_assertEqual({{1, 2}, 2}, ElementFun(5, ja_erl_utils_matrix:iterator(Matrix, #{row_order => left,  column_order => top,    loop_order => columns}))),
        ?_assertEqual({{1, 1}, 1}, ElementFun(5, ja_erl_utils_matrix:iterator(Matrix, #{row_order => right, column_order => top,    loop_order => columns}))),
        ?_assertEqual({{4, 2}, 8}, ElementFun(5, ja_erl_utils_matrix:iterator(Matrix, #{                    column_order => bottom, loop_order => columns}))),
        ?_assertEqual({{4, 2}, 8}, ElementFun(5, ja_erl_utils_matrix:iterator(Matrix, #{row_order => left,  column_order => bottom, loop_order => columns}))),
        ?_assertEqual({{4, 1}, 7}, ElementFun(5, ja_erl_utils_matrix:iterator(Matrix, #{row_order => right, column_order => bottom, loop_order => columns})))
    ].


fold_test_() ->
    Matrix = {matrix, 2, 2, undefined, #{{1,1}=>2,{1,2}=>3,{2,1}=>5,{2,2}=>7}},
    Fun = fun({R,C},V,A) -> A*100+R*C*V end,
    [
        ?_assertEqual( 2061028, ja_erl_utils_matrix:fold(Fun, 0, Matrix, #{                  })),
        ?_assertEqual( 2061028, ja_erl_utils_matrix:fold(Fun, 0, Matrix, #{row_order => left })),
        ?_assertEqual( 6022810, ja_erl_utils_matrix:fold(Fun, 0, Matrix, #{row_order => right})),

        ?_assertEqual( 2061028, ja_erl_utils_matrix:fold(Fun, 0, Matrix, #{column_order => top   })),
        ?_assertEqual(10280206, ja_erl_utils_matrix:fold(Fun, 0, Matrix, #{column_order => bottom})),

        ?_assertEqual(28100602, ja_erl_utils_matrix:fold(Fun, 0, Matrix, #{row_order => right, column_order => bottom})),

        ?_assertEqual( 2061028, ja_erl_utils_matrix:fold(Fun, 0, Matrix, #{loop_order => rows                                               })),
        ?_assertEqual( 2100628, ja_erl_utils_matrix:fold(Fun, 0, Matrix, #{loop_order => columns                                            })),
        ?_assertEqual( 6280210, ja_erl_utils_matrix:fold(Fun, 0, Matrix, #{loop_order => columns, row_order => right                        })),
        ?_assertEqual(10022806, ja_erl_utils_matrix:fold(Fun, 0, Matrix, #{loop_order => columns,                     column_order => bottom})),
        ?_assertEqual(28061002, ja_erl_utils_matrix:fold(Fun, 0, Matrix, #{loop_order => columns, row_order => right, column_order => bottom})),

        ?_assertEqual( 20601102802, ja_erl_utils_matrix:fold(Fun, 0, Matrix, #{loop_end_fun => fun(R,A) -> A*100+R end                                                                   })),
        ?_assertEqual( 20601102802, ja_erl_utils_matrix:fold(Fun, 0, Matrix, #{loop_end_fun => fun(R,A) -> A*100+R end, loop_order => rows                                               })),
        ?_assertEqual( 21001062802, ja_erl_utils_matrix:fold(Fun, 0, Matrix, #{loop_end_fun => fun(C,A) -> A*100+C end, loop_order => columns                                            })),
        ?_assertEqual( 62802021001, ja_erl_utils_matrix:fold(Fun, 0, Matrix, #{loop_end_fun => fun(C,A) -> A*100+C end, loop_order => columns, row_order => right                        })),
        ?_assertEqual(100201280602, ja_erl_utils_matrix:fold(Fun, 0, Matrix, #{loop_end_fun => fun(C,A) -> A*100+C end, loop_order => columns,                     column_order => bottom})),
        ?_assertEqual(280602100201, ja_erl_utils_matrix:fold(Fun, 0, Matrix, #{loop_end_fun => fun(C,A) -> A*100+C end, loop_order => columns, row_order => right, column_order => bottom}))
    ].


foldf_test_() ->
    Matrix = {matrix, 2, 2, undefined, #{{1,1}=>2,{1,2}=>3,{2,1}=>5,{2,2}=>7}},
    [
        ?_assertEqual(1010202, ja_erl_utils_matrix:foldf(fun({R,_},_,A) -> A*100+R     end, 0, Matrix)),
        ?_assertEqual(1020102, ja_erl_utils_matrix:foldf(fun({_,C},_,A) -> A*100+C     end, 0, Matrix)),
        ?_assertEqual(2030507, ja_erl_utils_matrix:foldf(fun({_,_},V,A) -> A*100+V     end, 0, Matrix)),
        ?_assertEqual(2061028, ja_erl_utils_matrix:foldf(fun({R,C},V,A) -> A*100+R*C*V end, 0, Matrix))
    ].


foldb_test_() ->
    Matrix = {matrix, 2, 2, undefined, #{{1,1}=>2,{1,2}=>3,{2,1}=>5,{2,2}=>7}},
    [
        ?_assertEqual( 2020101, ja_erl_utils_matrix:foldb(fun({R,_},_,A) -> A*100+R     end, 0, Matrix)),
        ?_assertEqual( 2010201, ja_erl_utils_matrix:foldb(fun({_,C},_,A) -> A*100+C     end, 0, Matrix)),
        ?_assertEqual( 7050302, ja_erl_utils_matrix:foldb(fun({_,_},V,A) -> A*100+V     end, 0, Matrix)),
        ?_assertEqual(28100602, ja_erl_utils_matrix:foldb(fun({R,C},V,A) -> A*100+R*C*V end, 0, Matrix))
    ].


map_test_() ->
    Matrix1 = {matrix, 1, 1, 9, #{{1,1}=>5}},
    Matrix2 = {matrix, 1, 2, 9, #{{1,1}=>3,{1,2}=>7}},
    Matrix3 = {matrix, 2, 1, 9, #{{1,1}=>4,{2,1}=>6}},
    Matrix4 = {matrix, 2, 3, 9, #{{1,1}=>1,         {1,3}=>3,{2,1}=>4,{2,2}=>5         }},
    Matrix5 = {matrix, 2, 3, 9, #{{1,1}=>1,{1,2}=>2,{1,3}=>3,{2,1}=>4,{2,2}=>5,{2,3}=>6}},
    Fun = fun({Row, Column}, Value) -> Row*100 + Column*10 + Value end,
    [
        ?_assertEqual(erlang:setelement(5, Matrix1, #{{1,1}=>115}),                                                        ja_erl_utils_matrix:map(Fun, Matrix1)),
        ?_assertEqual(erlang:setelement(5, Matrix2, #{{1,1}=>113,{1,2}=>127}),                                             ja_erl_utils_matrix:map(Fun, Matrix2)),
        ?_assertEqual(erlang:setelement(5, Matrix3, #{{1,1}=>114,{2,1}=>216}),                                             ja_erl_utils_matrix:map(Fun, Matrix3)),
        ?_assertEqual(erlang:setelement(5, Matrix4, #{{1,1}=>111,{1,2}=>129,{1,3}=>133,{2,1}=>214,{2,2}=>225,{2,3}=>239}), ja_erl_utils_matrix:map(Fun, Matrix4)),
        ?_assertEqual(erlang:setelement(5, Matrix5, #{{1,1}=>111,{1,2}=>122,{1,3}=>133,{2,1}=>214,{2,2}=>225,{2,3}=>236}), ja_erl_utils_matrix:map(Fun, Matrix5))
    ].


map_sum_test_() ->
    Fun = fun({Row, Column}, Value) -> Row*100 + Column*10 + Value end,
    [
        ?_assertEqual( 116, ja_erl_utils_matrix:map_sum(Fun, {matrix, 1, 1, 9, #{{1, 1} => 6}})),
        ?_assertEqual(1051, ja_erl_utils_matrix:map_sum(Fun, {matrix, 2, 3, 9, #{{1,1}=>1,{1,2}=>2,         {2,1}=>4,         {2,3}=>6}})),
        ?_assertEqual(1041, ja_erl_utils_matrix:map_sum(Fun, {matrix, 2, 3, 9, #{{1,1}=>1,{1,2}=>2,{1,3}=>3,{2,1}=>4,{2,2}=>5,{2,3}=>6}}))
    ].


map_sum_foldf_test_() ->
    Fun = fun({Row, Column}, Value, Acc) -> Sum = Row*100 + Column*10 + Value, {Sum, Acc*1000 + Sum} end,
    [
        ?_assertEqual({ 116, 116},                ja_erl_utils_matrix:map_sum_foldf(Fun, 0, {matrix, 1, 1, 9, #{{1, 1} => 6}})),
        ?_assertEqual({1051, 111122139214229236}, ja_erl_utils_matrix:map_sum_foldf(Fun, 0, {matrix, 2, 3, 9, #{{1,1}=>1,{1,2}=>2,         {2,1}=>4,         {2,3}=>6}})),
        ?_assertEqual({1041, 111122133214225236}, ja_erl_utils_matrix:map_sum_foldf(Fun, 0, {matrix, 2, 3, 9, #{{1,1}=>1,{1,2}=>2,{1,3}=>3,{2,1}=>4,{2,2}=>5,{2,3}=>6}}))
    ].


filter_count_test_() ->
    Fun = fun({Row, Column}, Value) -> Row*Column*Value > 10 end,
    [
        ?_assertEqual(0, ja_erl_utils_matrix:filter_count(Fun, {matrix, 1, 1, 9, #{{1, 1} =>  6}})),
        ?_assertEqual(1, ja_erl_utils_matrix:filter_count(Fun, {matrix, 1, 1, 9, #{{1, 1} => 11}})),
        ?_assertEqual(4, ja_erl_utils_matrix:filter_count(Fun, {matrix, 2, 3, 9, #{{1,1}=>1,{1,2}=>8,         {2,1}=>4,         {2,3}=>6}})),
        ?_assertEqual(2, ja_erl_utils_matrix:filter_count(Fun, {matrix, 2, 3, 9, #{{1,1}=>1,{1,2}=>8,{1,3}=>3,{2,1}=>4,{2,2}=>1,{2,3}=>6}}))
    ].


filter_count_foldf_test_() ->
    Fun = fun({Row, Column}, Value, Acc) -> {Row*Column*Value > 10, Acc*1000 + Row*100 + Column*10 + Value} end,
    [
        ?_assertEqual({0, 116},                ja_erl_utils_matrix:filter_count_foldf(Fun, 0, {matrix, 1, 1, 9, #{{1, 1} =>  6}})),
        ?_assertEqual({1, 121},                ja_erl_utils_matrix:filter_count_foldf(Fun, 0, {matrix, 1, 1, 9, #{{1, 1} => 11}})),
        ?_assertEqual({4, 111128139214229236}, ja_erl_utils_matrix:filter_count_foldf(Fun, 0, {matrix, 2, 3, 9, #{{1,1}=>1,{1,2}=>8,         {2,1}=>4,         {2,3}=>6}})),
        ?_assertEqual({2, 111128133214221236}, ja_erl_utils_matrix:filter_count_foldf(Fun, 0, {matrix, 2, 3, 9, #{{1,1}=>1,{1,2}=>8,{1,3}=>3,{2,1}=>4,{2,2}=>1,{2,3}=>6}}))
    ].
