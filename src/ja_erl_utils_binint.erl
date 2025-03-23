%%%
%%% Copyright 2024-2025, Julius Andrikonis <julius@andrikonis.lt>.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License"); you may not use
%%% this file except in compliance with the License. You may obtain a copy of the
%%% License at
%%%
%%% http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software distributed
%%% under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
%%% CONDITIONS OF ANY KIND, either express or implied. See the License for the
%%% specific language governing permissions and limitations under the License.
%%%

-module(ja_erl_utils_binint).
-moduledoc "Module for working with binary integers and bits.".
-export([from_integer/1, to_integer/1]).
-export([bit_and/2, bit_or/2, bit_xor/2, bit_invert/1]).
-export([and_/2, or_/2, xor_/2, invert/1]).     % NOTE `and_` instead of `and`, because `and` is a reserved word and cannot be used for function name.
-export([drop_insignificant_zeros/1]).
-export_type([bit/0, binint/0]).

-define(IS_BIT(B), (B =:= 0 orelse B =:= 1)).


%------------------------------------------------------------------------------
% Type definitions
%------------------------------------------------------------------------------


%%
-doc "Represents one bit.".
-type bit() :: 0 | 1.


%%
-doc """
    Represents a binary integer number.

    The list of bits starts with the least significant bit and ends with the
    most significant one.
    """.
-type binint() :: [bit()].


%------------------------------------------------------------------------------
% Functions of the module
%------------------------------------------------------------------------------


%%
-doc "Returns binary representation of integer.".
-spec from_integer(Integer :: integer()) -> BinInt :: binint().

from_integer(Int) when Int >= 0 -> from_integer(Int, []).
from_integer(0,   Acc)              -> lists:reverse(Acc);
from_integer(Int, Acc) when Int > 0 -> from_integer(Int div 2, [Int rem 2 | Acc]).


%%
-doc "Returns integer from its binary representation.".
-spec to_integer(BinInt :: binint()) -> Integer :: integer().

to_integer(BinInt) -> to_integer(lists:reverse(BinInt), 0).
to_integer([], Acc)                           -> Acc;
to_integer([Bit|Bits], Acc) when ?IS_BIT(Bit) -> to_integer(Bits, Acc*2+Bit).


%%
-doc """
    Performs `and` operation on two bits.

    In addition to Erlang's `band` operator, it checks if parameters are bits.
    """.
-spec bit_and(Bit1 :: bit(), Bit2 :: bit()) -> Bit1AndBit2 :: bit().

bit_and(Bit1, Bit2) -> bit_op(op_and, Bit1, Bit2).


%%
-doc """
    Performs `or` operation on two bits.

    In addition to Erlang's `bor` operator, it checks if parameters are bits.
    """.
-spec bit_or(Bit1 :: bit(), Bit2 :: bit()) -> Bit1OrBit2 :: bit().

bit_or(Bit1, Bit2) -> bit_op(op_or, Bit1, Bit2).


%%
-doc """
    Performs `xor` operation on two bits.

    In addition to Erlang's `bxor` operator, it checks if parameters are bits.
    """.
-spec bit_xor(Bit1 :: bit(), Bit2 :: bit()) -> Bit1XorBit2 :: bit().

bit_xor(Bit1, Bit2) -> bit_op(op_xor, Bit1, Bit2).


%%
-doc "Inverts a single bit.".
-spec bit_invert(Bit :: bit()) -> InvertedBit :: bit().

bit_invert(1) -> 0;
bit_invert(0) -> 1.


%%
-doc "Performs bitwise `and` operation on two binary integers.".
-spec and_(BinInt1 :: binint(), BinInt2 :: binint()) -> BinInt1AndBinInt2 :: binint().

and_(BinInt1, BinInt2) -> op_(op_and, BinInt1, BinInt2).


%%
-doc "Performs bitwise `or` operation on two binary integers.".
-spec or_(BinInt1 :: binint(), BinInt2 :: binint()) -> BinInt1OrBinInt2 :: binint().

or_(BinInt1, BinInt2) -> op_(op_or, BinInt1, BinInt2).


%%
-doc "Performs bitwise `xor` operation on two binary integers.".
-spec xor_(BinInt1 :: binint(), BinInt2 :: binint()) -> BinInt1XorBinInt2 :: binint().

xor_(BinInt1, BinInt2) -> op_(op_xor, BinInt1, BinInt2).


%%
-doc """
    Inverts bits of a binary integer.

    The resulting binary integer might contain 0 as the most significant number.
    Use `drop_insignificant_zeros/1` to remove them.
    """.
-spec invert(BinInt :: binint()) -> BinIntInverted :: binint().

invert(BinInt) -> lists:map(fun bit_invert/1, BinInt).


%%
-doc """
    Drops insignificant 0s (the ones, that are in the most significant positions)
    of binary integer.
    """.
-spec drop_insignificant_zeros(BinInt :: binint()) -> Result :: binint().

drop_insignificant_zeros(BinInt) -> lists:reverse(drop_start_zeros(lists:reverse(BinInt))).




%% -----------------------------------------------------------------
%%  Internal functions (used by more than one function)
%% -----------------------------------------------------------------



%%
%%
%%
bit_op(op_and, Bit1, Bit2) when ?IS_BIT(Bit1), ?IS_BIT(Bit2) -> Bit1 band Bit2;
bit_op(op_or,  Bit1, Bit2) when ?IS_BIT(Bit1), ?IS_BIT(Bit2) -> Bit1 bor  Bit2;
bit_op(op_xor, Bit1, Bit2) when ?IS_BIT(Bit1), ?IS_BIT(Bit2) -> Bit1 bxor Bit2.


%%
%%
%%
op_(Op, BinInt1, BinInt2) -> op_(Op, BinInt1, BinInt2, []).

op_(_Op, [],           [],           Acc)                                   -> lists:reverse(drop_start_zeros(Acc));
op_( Op, [Bit1|Bits1], [Bit2|Bits2], Acc) when ?IS_BIT(Bit1), ?IS_BIT(Bit2) -> op_(Op, Bits1, Bits2, [bit_op(Op, Bit1, Bit2) | Acc]);
op_( Op, [],           [Bit2|Bits2], Acc) when ?IS_BIT(Bit2)                -> op_(Op, [],    Bits2, [bit_op(Op, 0,    Bit2) | Acc]);
op_( Op, [Bit1|Bits1], [],           Acc) when ?IS_BIT(Bit1)                -> op_(Op, Bits1, [],    [bit_op(Op, Bit1, 0   ) | Acc]).


%%
%%
%%
drop_start_zeros([0])      -> [0];
drop_start_zeros([0|Bits]) -> drop_start_zeros(Bits);
drop_start_zeros([1|Bits]) -> [1|Bits].
