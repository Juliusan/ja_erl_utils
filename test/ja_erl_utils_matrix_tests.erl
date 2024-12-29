%%%
%%% Unit tests for ja_erl_utils_matrix.
%%%
-module(ja_erl_utils_matrix_tests).
-include_lib("eunit/include/eunit.hrl").


get_new_matrix_test_() ->
    [
        ?_assertEqual(#{},                                             ja_erl_utils_matrix:get_new_matrix(a,       {0, 0})),
        ?_assertEqual(#{{1,1}=>a},                                     ja_erl_utils_matrix:get_new_matrix(a,       {1, 1})),
        ?_assertEqual(#{{1,1}=>5,      {1,2}=>5,      {1,3}=>5      }, ja_erl_utils_matrix:get_new_matrix(5,       {1, 3})),
        ?_assertEqual(#{{1,1}=>$x,     {2,1}=>$x},                     ja_erl_utils_matrix:get_new_matrix($x,      {2, 1})),
        ?_assertEqual(#{{1,1}=>"a",    {1,2}=>"a",    {1,3}=>"a",
                        {2,1}=>"a",    {2,2}=>"a",    {2,3}=>"a"    }, ja_erl_utils_matrix:get_new_matrix("a",     {2, 3})),
        ?_assertEqual(#{{1,1}=><<"a">>,{1,2}=><<"a">>,{1,3}=><<"a">>,
                        {2,1}=><<"a">>,{2,2}=><<"a">>,{2,3}=><<"a">>,
                        {3,1}=><<"a">>,{3,2}=><<"a">>,{3,3}=><<"a">>}, ja_erl_utils_matrix:get_new_matrix(<<"a">>, {3, 3}))
    ].


get_char_matrix_test_() ->
    [
        ?_assertEqual({#{},                              {0, 0}}, ja_erl_utils_matrix:get_char_matrix([])),
        ?_assertEqual({#{{1,1}=>$a},                     {1, 1}}, ja_erl_utils_matrix:get_char_matrix(["a"])),
        ?_assertEqual({#{{1,1}=>$a,{1,2}=>$b,{1,3}=>$c}, {1, 3}}, ja_erl_utils_matrix:get_char_matrix(["abc"])),
        ?_assertEqual({#{{1,1}=>$a,{2,1}=>$b},           {2, 1}}, ja_erl_utils_matrix:get_char_matrix(["a", "b"])),
        ?_assertEqual({#{{1,1}=>$a,{1,2}=>$b,{1,3}=>$c,
                         {2,1}=>$d,{2,2}=>$e,{2,3}=>$f}, {2, 3}}, ja_erl_utils_matrix:get_char_matrix(["abc","def"])),
        ?_assertEqual({#{{1,1}=>$a,{1,2}=>$b,{1,3}=>$c,
                         {2,1}=>$d,{2,2}=>$e,{2,3}=>$f,
                         {3,1}=>$g,{3,2}=>$h,{3,3}=>$i}, {3, 3}}, ja_erl_utils_matrix:get_char_matrix(["abc","def","ghi"]))
    ].


matrix_index_of_test_() ->
    [
        ?_assertEqual(undefined, ja_erl_utils_matrix:matrix_index_of(a, #{}          )),
        ?_assertEqual({1,1},     ja_erl_utils_matrix:matrix_index_of(a, #{{1,1}=>a,{1,2}=>b,{2,1}=>c,{2,2}=>d})),
        ?_assertEqual({1,2},     ja_erl_utils_matrix:matrix_index_of(b, #{{1,1}=>a,{1,2}=>b,{2,1}=>c,{2,2}=>d})),
        ?_assertEqual({2,1},     ja_erl_utils_matrix:matrix_index_of(c, #{{1,1}=>a,{1,2}=>b,{2,1}=>c,{2,2}=>d})),
        ?_assertEqual({2,2},     ja_erl_utils_matrix:matrix_index_of(d, #{{1,1}=>a,{1,2}=>b,{2,1}=>c,{2,2}=>d})),
        ?_assertEqual(undefined, ja_erl_utils_matrix:matrix_index_of(e, #{{1,1}=>a,{1,2}=>b,{2,1}=>c,{2,2}=>d}))
    ].


matrix_is_valid_index_test_() ->
    [
        ?_assertEqual(true,  ja_erl_utils_matrix:matrix_is_valid_index({2,3}, {8,8})),
        ?_assertEqual(true,  ja_erl_utils_matrix:matrix_is_valid_index({1,1}, {8,8})),
        ?_assertEqual(true,  ja_erl_utils_matrix:matrix_is_valid_index({1,5}, {8,8})),
        ?_assertEqual(true,  ja_erl_utils_matrix:matrix_is_valid_index({1,8}, {8,8})),
        ?_assertEqual(true,  ja_erl_utils_matrix:matrix_is_valid_index({5,8}, {8,8})),
        ?_assertEqual(true,  ja_erl_utils_matrix:matrix_is_valid_index({8,8}, {8,8})),
        ?_assertEqual(true,  ja_erl_utils_matrix:matrix_is_valid_index({8,5}, {8,8})),
        ?_assertEqual(true,  ja_erl_utils_matrix:matrix_is_valid_index({8,1}, {8,8})),
        ?_assertEqual(true,  ja_erl_utils_matrix:matrix_is_valid_index({5,1}, {8,8})),
        ?_assertEqual(false, ja_erl_utils_matrix:matrix_is_valid_index({0,0}, {8,8})),
        ?_assertEqual(false, ja_erl_utils_matrix:matrix_is_valid_index({0,5}, {8,8})),
        ?_assertEqual(false, ja_erl_utils_matrix:matrix_is_valid_index({0,9}, {8,8})),
        ?_assertEqual(false, ja_erl_utils_matrix:matrix_is_valid_index({5,9}, {8,8})),
        ?_assertEqual(false, ja_erl_utils_matrix:matrix_is_valid_index({9,9}, {8,8})),
        ?_assertEqual(false, ja_erl_utils_matrix:matrix_is_valid_index({9,5}, {8,8})),
        ?_assertEqual(false, ja_erl_utils_matrix:matrix_is_valid_index({9,0}, {8,8})),
        ?_assertEqual(false, ja_erl_utils_matrix:matrix_is_valid_index({5,0}, {8,8}))
    ].


matrix_next_index_test_() ->
    [
        ?_assertEqual({1,3},     ja_erl_utils_matrix:matrix_next_index({2,3}, up,    {8,8})),
        ?_assertEqual({2,4},     ja_erl_utils_matrix:matrix_next_index({2,3}, right, {8,8})),
        ?_assertEqual({3,3},     ja_erl_utils_matrix:matrix_next_index({2,3}, down,  {8,8})),
        ?_assertEqual({2,2},     ja_erl_utils_matrix:matrix_next_index({2,3}, left,  {8,8})),
        ?_assertEqual(undefined, ja_erl_utils_matrix:matrix_next_index({1,1}, up,    {8,8})),
        ?_assertEqual({1,2},     ja_erl_utils_matrix:matrix_next_index({1,1}, right, {8,8})),
        ?_assertEqual({2,1},     ja_erl_utils_matrix:matrix_next_index({1,1}, down,  {8,8})),
        ?_assertEqual(undefined, ja_erl_utils_matrix:matrix_next_index({1,1}, left,  {8,8})),
        ?_assertEqual(undefined, ja_erl_utils_matrix:matrix_next_index({1,5}, up,    {8,8})),
        ?_assertEqual({1,6},     ja_erl_utils_matrix:matrix_next_index({1,5}, right, {8,8})),
        ?_assertEqual({2,5},     ja_erl_utils_matrix:matrix_next_index({1,5}, down,  {8,8})),
        ?_assertEqual({1,4},     ja_erl_utils_matrix:matrix_next_index({1,5}, left,  {8,8})),
        ?_assertEqual(undefined, ja_erl_utils_matrix:matrix_next_index({1,8}, up,    {8,8})),
        ?_assertEqual(undefined, ja_erl_utils_matrix:matrix_next_index({1,8}, right, {8,8})),
        ?_assertEqual({2,8},     ja_erl_utils_matrix:matrix_next_index({1,8}, down,  {8,8})),
        ?_assertEqual({1,7},     ja_erl_utils_matrix:matrix_next_index({1,8}, left,  {8,8})),
        ?_assertEqual({4,8},     ja_erl_utils_matrix:matrix_next_index({5,8}, up,    {8,8})),
        ?_assertEqual(undefined, ja_erl_utils_matrix:matrix_next_index({5,8}, right, {8,8})),
        ?_assertEqual({6,8},     ja_erl_utils_matrix:matrix_next_index({5,8}, down,  {8,8})),
        ?_assertEqual({5,7},     ja_erl_utils_matrix:matrix_next_index({5,8}, left,  {8,8})),
        ?_assertEqual({7,8},     ja_erl_utils_matrix:matrix_next_index({8,8}, up,    {8,8})),
        ?_assertEqual(undefined, ja_erl_utils_matrix:matrix_next_index({8,8}, right, {8,8})),
        ?_assertEqual(undefined, ja_erl_utils_matrix:matrix_next_index({8,8}, down,  {8,8})),
        ?_assertEqual({8,7},     ja_erl_utils_matrix:matrix_next_index({8,8}, left,  {8,8})),
        ?_assertEqual({7,5},     ja_erl_utils_matrix:matrix_next_index({8,5}, up,    {8,8})),
        ?_assertEqual({8,6},     ja_erl_utils_matrix:matrix_next_index({8,5}, right, {8,8})),
        ?_assertEqual(undefined, ja_erl_utils_matrix:matrix_next_index({8,5}, down,  {8,8})),
        ?_assertEqual({8,4},     ja_erl_utils_matrix:matrix_next_index({8,5}, left,  {8,8})),
        ?_assertEqual({7,1},     ja_erl_utils_matrix:matrix_next_index({8,1}, up,    {8,8})),
        ?_assertEqual({8,2},     ja_erl_utils_matrix:matrix_next_index({8,1}, right, {8,8})),
        ?_assertEqual(undefined, ja_erl_utils_matrix:matrix_next_index({8,1}, down,  {8,8})),
        ?_assertEqual(undefined, ja_erl_utils_matrix:matrix_next_index({8,1}, left,  {8,8})),
        ?_assertEqual({4,1},     ja_erl_utils_matrix:matrix_next_index({5,1}, up,    {8,8})),
        ?_assertEqual({5,2},     ja_erl_utils_matrix:matrix_next_index({5,1}, right, {8,8})),
        ?_assertEqual({6,1},     ja_erl_utils_matrix:matrix_next_index({5,1}, down,  {8,8})),
        ?_assertEqual(undefined, ja_erl_utils_matrix:matrix_next_index({5,1}, left,  {8,8}))
    ].


matrix_foldl_test_() ->
    [
        ?_assertEqual(ok,      ja_erl_utils_matrix:matrix_foldl(fun(_,    _,_    ) -> fail             end, ok,     #{},                                    {0, 0})),
        ?_assertEqual(29,      ja_erl_utils_matrix:matrix_foldl(fun({R,_},V,A    ) -> A+R*V            end, 0,      #{{1,1}=>2,{1,2}=>3,{2,1}=>5,{2,2}=>7}, {2, 2})),
        ?_assertEqual(27,      ja_erl_utils_matrix:matrix_foldl(fun({_,C},V,A    ) -> A+C*V            end, 0,      #{{1,1}=>2,{1,2}=>3,{2,1}=>5,{2,2}=>7}, {2, 2})),
        ?_assertEqual({156,5}, ja_erl_utils_matrix:matrix_foldl(fun({R,C},V,{A,M}) -> {A+M*C*R*V, M+1} end, {0, 1}, #{{1,1}=>2,{1,2}=>3,{2,1}=>5,{2,2}=>7}, {2, 2}))
    ].
