%%%
%%% Unit tests for ja_erl_utils_map.
%%%
-module(ja_erl_utils_map_tests).
-include_lib("eunit/include/eunit.hrl").


map_reverse_test_() ->
    [
        ?_assertEqual(#{},                                ja_erl_utils_map:map_reverse(#{})),
        ?_assertEqual(#{10 => 1},                         ja_erl_utils_map:map_reverse(#{1 => 10})),
        ?_assertEqual(#{10 => 1, 9 => 2, 8 => 3, 7 => 4}, ja_erl_utils_map:map_reverse(#{1 => 10, 2 => 9, 3 => 8, 4 => 7}))
    ].


map_max_value_test_() ->
    [
        ?_assertEqual(undefined, ja_erl_utils_map:map_max_value(#{})),
        ?_assertEqual(10,        ja_erl_utils_map:map_max_value(#{1 => 10})),
        ?_assertEqual(15,        ja_erl_utils_map:map_max_value(#{1 => 10, 2 => -6, 3 => 15, 4 => 7}))
    ].


map_min_value_test_() ->
    [
        ?_assertEqual(undefined, ja_erl_utils_map:map_min_value(#{})),
        ?_assertEqual(10,        ja_erl_utils_map:map_min_value(#{1 => 10})),
        ?_assertEqual(-6,        ja_erl_utils_map:map_min_value(#{1 => 10, 2 => -6, 3 => 15, 4 => 7}))
    ].


map_map_sum_test_() ->
    [
        ?_assertEqual(0,  ja_erl_utils_map:map_map_sum(fun(K, V) -> K*V end, #{})),
        ?_assertEqual(10, ja_erl_utils_map:map_map_sum(fun(K, V) -> K*V end, #{1 => 10})),
        ?_assertEqual(80, ja_erl_utils_map:map_map_sum(fun(K, V) -> K*V end, #{1 => 10, 2 => 9, 3 => 8, 4 => 7}))
    ].


map_map_count_test_() ->
    [
        ?_assertEqual(0, ja_erl_utils_map:map_map_count(fun(K, V) -> K*V rem 2 =:= 0 end, #{})),
        ?_assertEqual(0, ja_erl_utils_map:map_map_count(fun(K, V) -> K*V rem 2 =:= 0 end, #{1 => 11})),
        ?_assertEqual(1, ja_erl_utils_map:map_map_count(fun(K, V) -> K*V rem 2 =:= 0 end, #{1 => 10})),
        ?_assertEqual(3, ja_erl_utils_map:map_map_count(fun(K, V) -> K*V rem 2 =:= 0 end, #{1 => 10, 2 => 9, 3 => 7, 4 => 6}))
    ].
