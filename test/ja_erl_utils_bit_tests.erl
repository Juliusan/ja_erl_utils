%%%
%%% Unit tests for ja_erl_utils_bit.
%%%
-module(ja_erl_utils_bit_tests).
-include_lib("eunit/include/eunit.hrl").


integer_to_bits_test_() ->
    [
        ?_assertEqual([],              ja_erl_utils_bit:integer_to_bits(0)),
        ?_assertEqual([1],             ja_erl_utils_bit:integer_to_bits(1)),
        ?_assertEqual([0,1,0,1],       ja_erl_utils_bit:integer_to_bits(10)),
        ?_assertEqual([1,1,0,1,1,1,1], ja_erl_utils_bit:integer_to_bits(123))
    ].


bits_to_integer_test_() ->
    [
        ?_assertEqual(0,   ja_erl_utils_bit:bits_to_integer([])),
        ?_assertEqual(0,   ja_erl_utils_bit:bits_to_integer([0])),
        ?_assertEqual(1,   ja_erl_utils_bit:bits_to_integer([1])),
        ?_assertEqual(1,   ja_erl_utils_bit:bits_to_integer([1,0,0])),
        ?_assertEqual(10,  ja_erl_utils_bit:bits_to_integer([0,1,0,1])),
        ?_assertEqual(123, ja_erl_utils_bit:bits_to_integer([1,1,0,1,1,1,1]))
    ].


bit_and_test_() ->
    [
        ?_assertEqual(0, ja_erl_utils_bit:bit_and(0, 0)),
        ?_assertEqual(0, ja_erl_utils_bit:bit_and(0, 1)),
        ?_assertEqual(0, ja_erl_utils_bit:bit_and(1, 0)),
        ?_assertEqual(1, ja_erl_utils_bit:bit_and(1, 1))
    ].



bit_or_test_() ->
    [
        ?_assertEqual(0, ja_erl_utils_bit:bit_or(0, 0)),
        ?_assertEqual(1, ja_erl_utils_bit:bit_or(0, 1)),
        ?_assertEqual(1, ja_erl_utils_bit:bit_or(1, 0)),
        ?_assertEqual(1, ja_erl_utils_bit:bit_or(1, 1))
    ].


bit_xor_test_() ->
    [
        ?_assertEqual(0, ja_erl_utils_bit:bit_xor(0, 0)),
        ?_assertEqual(1, ja_erl_utils_bit:bit_xor(0, 1)),
        ?_assertEqual(1, ja_erl_utils_bit:bit_xor(1, 0)),
        ?_assertEqual(0, ja_erl_utils_bit:bit_xor(1, 1))
    ].


bits_and_test_() ->
    [
        ?_assertEqual([0],         ja_erl_utils_bit:bits_and([0,1],             [0,0,1]        )),
        ?_assertEqual([0,0,0,1],   ja_erl_utils_bit:bits_and([0,0,1,1],         [0,1,0,1]      )),
        ?_assertEqual([0,1],       ja_erl_utils_bit:bits_and([0,1,1],           [0,1,0,1,0,0,1])),
        ?_assertEqual([1,0,0,0,1], ja_erl_utils_bit:bits_and([1,0,0,1,1,0,1,1], [1,1,1,0,1]    ))
    ].


bits_or_test_() ->
    [
        ?_assertEqual([0],               ja_erl_utils_bit:bits_or([0],               [0]            )),
        ?_assertEqual([0,1,1,1],         ja_erl_utils_bit:bits_or([0,0,1,1],         [0,1,0,1]      )),
        ?_assertEqual([0,1,1,1,0,0,1],   ja_erl_utils_bit:bits_or([0,1,1],           [0,1,0,1,0,0,1])),
        ?_assertEqual([1,1,1,1,1,0,1,1], ja_erl_utils_bit:bits_or([1,0,0,1,1,0,1,1], [1,1,1,0,1]    ))
    ].


bits_xor_test_() ->
    [
        ?_assertEqual([0],               ja_erl_utils_bit:bits_xor([0,1],             [0,1]          )),
        ?_assertEqual([0,1,1],           ja_erl_utils_bit:bits_xor([0,0,1,1],         [0,1,0,1]      )),
        ?_assertEqual([0,0,1,1,0,0,1],   ja_erl_utils_bit:bits_xor([0,1,1],           [0,1,0,1,0,0,1])),
        ?_assertEqual([0,1,1,1,0,0,1,1], ja_erl_utils_bit:bits_xor([1,0,0,1,1,0,1,1], [1,1,1,0,1]    ))
    ].
