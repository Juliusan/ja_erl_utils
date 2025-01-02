%%%
%%% Unit tests for ja_erl_utils_bit.
%%%
-module(ja_erl_utils_binint_tests).
-include_lib("eunit/include/eunit.hrl").


from_integer_test_() ->
    [
        ?_assertEqual([],              ja_erl_utils_binint:from_integer(0)),
        ?_assertEqual([1],             ja_erl_utils_binint:from_integer(1)),
        ?_assertEqual([0,1,0,1],       ja_erl_utils_binint:from_integer(10)),
        ?_assertEqual([1,1,0,1,1,1,1], ja_erl_utils_binint:from_integer(123))
    ].


to_integer_test_() ->
    [
        ?_assertEqual(0,   ja_erl_utils_binint:to_integer([])),
        ?_assertEqual(0,   ja_erl_utils_binint:to_integer([0])),
        ?_assertEqual(1,   ja_erl_utils_binint:to_integer([1])),
        ?_assertEqual(1,   ja_erl_utils_binint:to_integer([1,0,0])),
        ?_assertEqual(10,  ja_erl_utils_binint:to_integer([0,1,0,1])),
        ?_assertEqual(123, ja_erl_utils_binint:to_integer([1,1,0,1,1,1,1]))
    ].


bit_and_test_() ->
    [
        ?_assertEqual(0, ja_erl_utils_binint:bit_and(0, 0)),
        ?_assertEqual(0, ja_erl_utils_binint:bit_and(0, 1)),
        ?_assertEqual(0, ja_erl_utils_binint:bit_and(1, 0)),
        ?_assertEqual(1, ja_erl_utils_binint:bit_and(1, 1))
    ].



bit_or_test_() ->
    [
        ?_assertEqual(0, ja_erl_utils_binint:bit_or(0, 0)),
        ?_assertEqual(1, ja_erl_utils_binint:bit_or(0, 1)),
        ?_assertEqual(1, ja_erl_utils_binint:bit_or(1, 0)),
        ?_assertEqual(1, ja_erl_utils_binint:bit_or(1, 1))
    ].


bit_xor_test_() ->
    [
        ?_assertEqual(0, ja_erl_utils_binint:bit_xor(0, 0)),
        ?_assertEqual(1, ja_erl_utils_binint:bit_xor(0, 1)),
        ?_assertEqual(1, ja_erl_utils_binint:bit_xor(1, 0)),
        ?_assertEqual(0, ja_erl_utils_binint:bit_xor(1, 1))
    ].


and__test_() ->
    [
        ?_assertEqual([0],         ja_erl_utils_binint:and_([0,1],             [0,0,1]        )),
        ?_assertEqual([0,0,0,1],   ja_erl_utils_binint:and_([0,0,1,1],         [0,1,0,1]      )),
        ?_assertEqual([0,1],       ja_erl_utils_binint:and_([0,1,1],           [0,1,0,1,0,0,1])),
        ?_assertEqual([1,0,0,0,1], ja_erl_utils_binint:and_([1,0,0,1,1,0,1,1], [1,1,1,0,1]    ))
    ].


or__test_() ->
    [
        ?_assertEqual([0],               ja_erl_utils_binint:or_([0],               [0]            )),
        ?_assertEqual([0,1,1,1],         ja_erl_utils_binint:or_([0,0,1,1],         [0,1,0,1]      )),
        ?_assertEqual([0,1,1,1,0,0,1],   ja_erl_utils_binint:or_([0,1,1],           [0,1,0,1,0,0,1])),
        ?_assertEqual([1,1,1,1,1,0,1,1], ja_erl_utils_binint:or_([1,0,0,1,1,0,1,1], [1,1,1,0,1]    ))
    ].


xor__test_() ->
    [
        ?_assertEqual([0],               ja_erl_utils_binint:xor_([0,1],             [0,1]          )),
        ?_assertEqual([0,1,1],           ja_erl_utils_binint:xor_([0,0,1,1],         [0,1,0,1]      )),
        ?_assertEqual([0,0,1,1,0,0,1],   ja_erl_utils_binint:xor_([0,1,1],           [0,1,0,1,0,0,1])),
        ?_assertEqual([0,1,1,1,0,0,1,1], ja_erl_utils_binint:xor_([1,0,0,1,1,0,1,1], [1,1,1,0,1]    ))
    ].


invert_test_() ->
    [
        ?_assertEqual([1],               ja_erl_utils_binint:invert([0]              )),
        ?_assertEqual([0],               ja_erl_utils_binint:invert([1]              )),
        ?_assertEqual([1,1,0,0],         ja_erl_utils_binint:invert([0,0,1,1]        )),
        ?_assertEqual([1,0,1,0,0,1,1,0], ja_erl_utils_binint:invert([0,1,0,1,1,0,0,1]))
    ].


drop_insignificant_zeros_test_() ->
    [
        ?_assertEqual([0],               ja_erl_utils_binint:drop_insignificant_zeros([0]                        )),
        ?_assertEqual([1],               ja_erl_utils_binint:drop_insignificant_zeros([1]                        )),
        ?_assertEqual([0],               ja_erl_utils_binint:drop_insignificant_zeros([0,0,0,0]                  )),
        ?_assertEqual([1],               ja_erl_utils_binint:drop_insignificant_zeros([1,0,0,0]                  )),
        ?_assertEqual([0,1,1,0,0,1,0,1], ja_erl_utils_binint:drop_insignificant_zeros([0,1,1,0,0,1,0,1]          )),
        ?_assertEqual([0,1,1,0,0,1,0,1], ja_erl_utils_binint:drop_insignificant_zeros([0,1,1,0,0,1,0,1,0]        )),
        ?_assertEqual([0,1,1,0,0,1,0,1], ja_erl_utils_binint:drop_insignificant_zeros([0,1,1,0,0,1,0,1,0,0,0,0,0]))
    ].
