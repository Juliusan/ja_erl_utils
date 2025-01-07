%%%
%%% Unit tests for ja_erl_utils_int.
%%%
-module(ja_erl_utils_int_tests).
-include_lib("eunit/include/eunit.hrl").


integer_digit_count_test_() ->
    [
        ?_assertEqual(1,  ja_erl_utils_int:integer_digit_count(                                        1)),
        ?_assertEqual(1,  ja_erl_utils_int:integer_digit_count(                                        2)),
        ?_assertEqual(1,  ja_erl_utils_int:integer_digit_count(                                        9)),
        ?_assertEqual(2,  ja_erl_utils_int:integer_digit_count(                                       10)),
        ?_assertEqual(2,  ja_erl_utils_int:integer_digit_count(                                       28)),
        ?_assertEqual(2,  ja_erl_utils_int:integer_digit_count(                                       98)),
        ?_assertEqual(2,  ja_erl_utils_int:integer_digit_count(                                       99)),
        ?_assertEqual(3,  ja_erl_utils_int:integer_digit_count(                                      100)),
        ?_assertEqual(3,  ja_erl_utils_int:integer_digit_count(                                      999)),
        ?_assertEqual(4,  ja_erl_utils_int:integer_digit_count(                                    1_000)),
        ?_assertEqual(15, ja_erl_utils_int:integer_digit_count(                      999_999_999_999_999)),
        ?_assertEqual(16, ja_erl_utils_int:integer_digit_count(                    1_000_000_000_000_000)),
        ?_assertEqual(30, ja_erl_utils_int:integer_digit_count(  999_999_999_999_999_999_999_999_999_999)),
        ?_assertEqual(31, ja_erl_utils_int:integer_digit_count(1_000_000_000_000_000_000_000_000_000_000))
    ].


integer_10_pow_test_() ->
    [
        ?_assertEqual(1,                                         ja_erl_utils_int:integer_10_pow( 0)),
        ?_assertEqual(10,                                        ja_erl_utils_int:integer_10_pow( 1)),
        ?_assertEqual(100,                                       ja_erl_utils_int:integer_10_pow( 2)),
        ?_assertEqual(1_000,                                     ja_erl_utils_int:integer_10_pow( 3)),
        ?_assertEqual(10_000_000_000,                            ja_erl_utils_int:integer_10_pow(10)),
        ?_assertEqual(100_000_000_000_000_000_000,               ja_erl_utils_int:integer_10_pow(20)),
        ?_assertEqual(1_000_000_000_000_000_000_000_000_000_000, ja_erl_utils_int:integer_10_pow(30))
    ].


concat_integers_test_() ->
    [
        ?_assertEqual(10,                             ja_erl_utils_int:concat_integers(1,            0      )),
        ?_assertEqual(321,                            ja_erl_utils_int:concat_integers(3,            21     )),
        ?_assertEqual(321,                            ja_erl_utils_int:concat_integers(32,           1      )),
        ?_assertEqual(999,                            ja_erl_utils_int:concat_integers(9,            99     )),
        ?_assertEqual(999,                            ja_erl_utils_int:concat_integers(99,           9      )),
        ?_assertEqual(12345678901234567890,           ja_erl_utils_int:concat_integers(1234567890123,4567890          )),
        ?_assertEqual(123456789012345678901234567890, ja_erl_utils_int:concat_integers(1,23456789012345678901234567890))
    ].


split_integer_test_() ->
    [
        ?_assertEqual({1,            0      },           ja_erl_utils_int:split_integer(10,                            1 )),
        ?_assertEqual({32,           1      },           ja_erl_utils_int:split_integer(321,                           1 )),
        ?_assertEqual({3,            21     },           ja_erl_utils_int:split_integer(321,                           2 )),
        ?_assertEqual({99,           9      },           ja_erl_utils_int:split_integer(999,                           1 )),
        ?_assertEqual({9,            99     },           ja_erl_utils_int:split_integer(999,                           2 )),
        ?_assertEqual({1234567890123,4567890},           ja_erl_utils_int:split_integer(12345678901234567890,          7 )),
        ?_assertEqual({12345678901234567890123456789,0}, ja_erl_utils_int:split_integer(123456789012345678901234567890,1 )),
        ?_assertEqual({1,23456789012345678901234567890}, ja_erl_utils_int:split_integer(123456789012345678901234567890,29))
    ].


euclidean_div_test_() ->
    [
        ?_assertEqual( 2, ja_erl_utils_int:euclidean_div( 7,  3)),
        ?_assertEqual(-2, ja_erl_utils_int:euclidean_div( 7, -3)),
        ?_assertEqual(-3, ja_erl_utils_int:euclidean_div(-7,  3)),
        ?_assertEqual( 3, ja_erl_utils_int:euclidean_div(-7, -3)),
        ?_assertEqual( 2, ja_erl_utils_int:euclidean_div( 8,  3)),
        ?_assertEqual(-2, ja_erl_utils_int:euclidean_div( 8, -3)),
        ?_assertEqual(-3, ja_erl_utils_int:euclidean_div(-8,  3)),
        ?_assertEqual( 3, ja_erl_utils_int:euclidean_div(-8, -3)),
        ?_assertEqual( 3, ja_erl_utils_int:euclidean_div( 9,  3)),
        ?_assertEqual(-3, ja_erl_utils_int:euclidean_div( 9, -3)),
        ?_assertEqual(-3, ja_erl_utils_int:euclidean_div(-9,  3)),
        ?_assertEqual( 3, ja_erl_utils_int:euclidean_div(-9, -3))
    ].


euclidean_rem_test_() ->
    [
        ?_assertEqual(1, ja_erl_utils_int:euclidean_rem( 7,  3)),
        ?_assertEqual(1, ja_erl_utils_int:euclidean_rem( 7, -3)),
        ?_assertEqual(2, ja_erl_utils_int:euclidean_rem(-7,  3)),
        ?_assertEqual(2, ja_erl_utils_int:euclidean_rem(-7, -3)),
        ?_assertEqual(2, ja_erl_utils_int:euclidean_rem( 8,  3)),
        ?_assertEqual(2, ja_erl_utils_int:euclidean_rem( 8, -3)),
        ?_assertEqual(1, ja_erl_utils_int:euclidean_rem(-8,  3)),
        ?_assertEqual(1, ja_erl_utils_int:euclidean_rem(-8, -3)),
        ?_assertEqual(0, ja_erl_utils_int:euclidean_rem( 9,  3)),
        ?_assertEqual(0, ja_erl_utils_int:euclidean_rem( 9, -3)),
        ?_assertEqual(0, ja_erl_utils_int:euclidean_rem(-9,  3)),
        ?_assertEqual(0, ja_erl_utils_int:euclidean_rem(-9, -3))
    ].


solve_one_equation_int_test_() ->
    [
        ?_assertEqual( 2,        ja_erl_utils_int:solve_one_equation_int({ 2,  4})),
        ?_assertEqual(-2,        ja_erl_utils_int:solve_one_equation_int({-3,  6})),
        ?_assertEqual(-3,        ja_erl_utils_int:solve_one_equation_int({ 2, -6})),
        ?_assertEqual( 3,        ja_erl_utils_int:solve_one_equation_int({-3, -9})),
        ?_assertEqual(undefined, ja_erl_utils_int:solve_one_equation_int({ 2,  7})),
        ?_assertEqual(undefined, ja_erl_utils_int:solve_one_equation_int({-3,  5})),
        ?_assertEqual(undefined, ja_erl_utils_int:solve_one_equation_int({ 4, -6})),
        ?_assertEqual(undefined, ja_erl_utils_int:solve_one_equation_int({-5, -8})),
        ?_assertEqual(any,       ja_erl_utils_int:solve_one_equation_int({ 0,  0})),
        ?_assertEqual(undefined, ja_erl_utils_int:solve_one_equation_int({ 0, 10}))
    ].


solve_two_equations_int_test_() ->
    [
        ?_assertEqual({3,   5            }, ja_erl_utils_int:solve_two_equations_int({2, 7, 41}, {4, 6, 42})),
        ?_assertEqual({3,   5            }, ja_erl_utils_int:solve_two_equations_int({4, 6, 42}, {2, 7, 41})),
        ?_assertEqual({5,   3            }, ja_erl_utils_int:solve_two_equations_int({7, 2, 41}, {6, 4, 42})),
        ?_assertEqual({5,   3            }, ja_erl_utils_int:solve_two_equations_int({6, 4, 42}, {7, 2, 41})),
        ?_assertEqual({3,   5            }, ja_erl_utils_int:solve_two_equations_int({0, 7, 35}, {4, 6, 42})),
        ?_assertEqual({3,   5            }, ja_erl_utils_int:solve_two_equations_int({4, 6, 42}, {0, 7, 35})),
        ?_assertEqual({5,   3            }, ja_erl_utils_int:solve_two_equations_int({7, 0, 35}, {6, 4, 42})),
        ?_assertEqual({5,   3            }, ja_erl_utils_int:solve_two_equations_int({6, 4, 42}, {7, 0, 35})),
        ?_assertEqual({any, 5            }, ja_erl_utils_int:solve_two_equations_int({0, 7, 35}, {0, 6, 30})),
        ?_assertEqual({5,   any          }, ja_erl_utils_int:solve_two_equations_int({7, 0, 35}, {6, 0, 30})),
        ?_assertEqual({3,   5            }, ja_erl_utils_int:solve_two_equations_int({0, 7, 35}, {4, 0, 12})),
        ?_assertEqual({3,   5            }, ja_erl_utils_int:solve_two_equations_int({4, 0, 12}, {0, 7, 35})),
        ?_assertEqual({any, "(5 - 3*x)/6"}, ja_erl_utils_int:solve_two_equations_int({0, 0,  0}, {3, 6,  5})),
        ?_assertEqual({any, "(5 - 3*x)/6"}, ja_erl_utils_int:solve_two_equations_int({3, 6,  5}, {0, 0,  0})),
        ?_assertEqual({any, 5            }, ja_erl_utils_int:solve_two_equations_int({0, 0,  0}, {0, 3, 15})),
        ?_assertEqual({any, 5            }, ja_erl_utils_int:solve_two_equations_int({0, 3, 15}, {0, 0,  0})),
        ?_assertEqual({5,   any          }, ja_erl_utils_int:solve_two_equations_int({0, 0,  0}, {3, 0, 15})),
        ?_assertEqual({5,   any          }, ja_erl_utils_int:solve_two_equations_int({3, 0, 15}, {0, 0,  0})),
        ?_assertEqual({any, any          }, ja_erl_utils_int:solve_two_equations_int({0, 0,  0}, {0, 0,  0})),
        ?_assertEqual({any, "(8 - 2*x)/3"}, ja_erl_utils_int:solve_two_equations_int({2, 3,  8}, {4, 6, 16})),
        ?_assertEqual(undefined,            ja_erl_utils_int:solve_two_equations_int({2, 3, 14}, {4, 5, 25})), % x=2.5; y=3
        ?_assertEqual(undefined,            ja_erl_utils_int:solve_two_equations_int({4, 5, 25}, {2, 3, 14})), % x=2.5; y=3
        ?_assertEqual(undefined,            ja_erl_utils_int:solve_two_equations_int({3, 2, 14}, {5, 4, 25})), % x=3; y=2.5
        ?_assertEqual(undefined,            ja_erl_utils_int:solve_two_equations_int({5, 4, 25}, {3, 2, 14})), % x=3; y=2.5
        ?_assertEqual(undefined,            ja_erl_utils_int:solve_two_equations_int({2, 8, 21}, {6, 4, 18})), % x=1.5; y=2.25
        ?_assertEqual(undefined,            ja_erl_utils_int:solve_two_equations_int({6, 4, 18}, {2, 8, 21})), % x=1.5; y=2.25
        ?_assertEqual(undefined,            ja_erl_utils_int:solve_two_equations_int({8, 2, 21}, {4, 6, 18})), % x=2.25; y=1.5
        ?_assertEqual(undefined,            ja_erl_utils_int:solve_two_equations_int({4, 6, 18}, {8, 2, 21})), % x=2.25; y=1.5
        ?_assertEqual(undefined,            ja_erl_utils_int:solve_two_equations_int({0, 7, 35}, {0, 6, 24})),
        ?_assertEqual(undefined,            ja_erl_utils_int:solve_two_equations_int({7, 0, 35}, {6, 0, 24}))
    ].
