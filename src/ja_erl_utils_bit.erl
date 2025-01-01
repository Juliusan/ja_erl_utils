%%% @doc
%%% Module for working with binary numbers.
-module(ja_erl_utils_bit).
-export([integer_to_bits/1, bits_to_integer/1]).
-export([bit_and/2, bit_or/2, bit_xor/2, bit_invert/1]).
-export([bits_and/2, bits_or/2, bits_xor/2, bits_invert/1]).
-export([drop_insignificant_zeros/1]).

-define(IS_BIT(B), (B =:= 0 orelse B =:= 1)).

-include("ja_erl_utils_bit.hrl").

%%%
%%% In this modules bit is represented as Erlang integer 0 or 1.
%%% Bit list (bits) is started by least significant and ended by most significant bit.
%%%

%%
%%  Returns binary representation of integer as list of 0s and 1s. The first
%%  element in the returned list is a least significant digit and the last one
%%  is the most significant digit.
%%
-spec integer_to_bits(integer()) -> bits().

integer_to_bits(Int) when Int >= 0 -> integer_to_bits(Int, []).
integer_to_bits(0,   Acc)              -> lists:reverse(Acc);
integer_to_bits(Int, Acc) when Int > 0 -> integer_to_bits(Int div 2, [Int rem 2 | Acc]).


%%
%%  Returns integer from its binary representation, provided as list of 0s and
%%  1s. The first element in the parameter list is a least significant digit
%%  and the last one is the most significant digit.
%%
-spec bits_to_integer(bits()) -> integer().

bits_to_integer(Bits) -> bits_to_integer(lists:reverse(Bits), 0).

bits_to_integer([], Acc)                           -> Acc;
bits_to_integer([Bit|Bits], Acc) when ?IS_BIT(Bit) -> bits_to_integer(Bits, Acc*2+Bit).


%%
%%  Performs and operation for two bits. Compared to band, it checks if
%%  parameters are bits.
%%
-spec bit_and(bit(), bit()) -> bit().

bit_and(D1, D2) -> bit_op(op_and, D1, D2).


%%
%%  Performs or operation for two bits. Compared to bor, it checks if
%%  parameters are bits.
%%
-spec bit_or(bit(), bit()) -> bit().

bit_or(D1, D2) -> bit_op(op_or, D1, D2).


%%
%%  Performs xor operation for two bits. Compared to bxor, it checks if
%%  parameters are bits.
%%
-spec bit_xor(bit(), bit()) -> bit().

bit_xor(D1, D2) -> bit_op(op_xor, D1, D2).


%%
%%  Performs bitwise and operation for two binary numbers.
%%
-spec bits_and(bits(), bits()) -> bits().

bits_and(Bits1, Bits2) -> bits_op(op_and, Bits1, Bits2).


%%
%%  Performs bitwise or operation for two binary numbers.
%%
-spec bits_or(bits(), bits()) -> bits().

bits_or(Bits1, Bits2) -> bits_op(op_or, Bits1, Bits2).


%%
%%  Performs bitwise xor operation for two binary numbers.
%%
-spec bits_xor(bits(), bits()) -> bits().

bits_xor(Bits1, Bits2) -> bits_op(op_xor, Bits1, Bits2).


%%
%%  Inverts a single bit.
%%
-spec bit_invert(bit()) -> bit().

bit_invert(1) -> 0;
bit_invert(0) -> 1.


%%
%%  Inverts bits in a bit list. NOTE: the resulting bit list might contain 0 as
%%  the most significant number. Use drop_insignificant_zeros/1 for that.
%%
-spec bits_invert(bits()) -> bits().

bits_invert(Bits) -> lists:map(fun bit_invert/1, Bits).


%%
%%  Drops insignificant 0s (the ones, that are in the most significant positions
%%  of binary number; that are in the end of bit list).
%%
-spec drop_insignificant_zeros(bits()) -> bits().

drop_insignificant_zeros(Bits) -> lists:reverse(drop_start_zeros(lists:reverse(Bits))).




%% -----------------------------------------------------------------
%%  Internal functions (used by more than one function)
%% -----------------------------------------------------------------



%%
%%
%%
bit_op(op_and, D1, D2) when ?IS_BIT(D1), ?IS_BIT(D2) -> D1 band D2;
bit_op(op_or,  D1, D2) when ?IS_BIT(D1), ?IS_BIT(D2) -> D1 bor  D2;
bit_op(op_xor, D1, D2) when ?IS_BIT(D1), ?IS_BIT(D2) -> D1 bxor D2.


%%
%%
%%
bits_op(Op, Bits1, Bits2) -> bits_op(Op, Bits1, Bits2, []).

bits_op(_Op, [],           [],           Acc)                                   -> lists:reverse(drop_start_zeros(Acc));
bits_op( Op, [Bit1|Bits1], [Bit2|Bits2], Acc) when ?IS_BIT(Bit1), ?IS_BIT(Bit2) -> bits_op(Op, Bits1, Bits2, [bit_op(Op, Bit1, Bit2) | Acc]);
bits_op( Op, [],           [Bit2|Bits2], Acc) when ?IS_BIT(Bit2)                -> bits_op(Op, [],    Bits2, [bit_op(Op, 0,    Bit2) | Acc]);
bits_op( Op, [Bit1|Bits1], [],           Acc) when ?IS_BIT(Bit1)                -> bits_op(Op, Bits1, [],    [bit_op(Op, Bit1, 0   ) | Acc]).


%%
%%
%%
drop_start_zeros([0])        -> [0];
drop_start_zeros([0|Digits]) -> drop_start_zeros(Digits);
drop_start_zeros([1|Digits]) -> [1|Digits].
