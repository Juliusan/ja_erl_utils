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

-module(ja_erl_utils_int).
-moduledoc "Module for working with integer numbers.".
-export([digit_count/1, ten_pow/1, concat/2, split/2]).
-export([euclidean_div/2, euclidean_rem/2]).
-export([solve_one_equation/1, solve_two_equations/2]).


-define(CUT_INTEGER,       1_000_000_000_000).
-define(CUT_INTEGER_POWER, 12).


%%
-doc """
    Returns the number of digits in non negative integer.

    To obtain the result, this function uses `math:log10/1`, which returns a
    floating point number. Converting floats to integers might cause errors due
    to loss of precision. To combat this, `math:log10/1` is applied only to
    sufficiently small numbers (less than `10^12`). Larger numbers are divided
    (using integer division, which is lossless), until they fit this interval.

    TODO: Try recursive solution with single digit step, accumulating the result:
        fun(Int, Number, Power) when Int < Number -> Power;
        fun(Int, Number, Power) _                 -> fun(Int, Number*10, Power+1).
    """.
-spec digit_count(Integer :: integer()) -> DigitCount :: integer().

digit_count(0  )                                   -> 1;
digit_count(Int) when 0 < Int, Int <  ?CUT_INTEGER -> erlang:floor(math:log10(Int)) + 1;
digit_count(Int) when          Int >= ?CUT_INTEGER -> ?CUT_INTEGER_POWER + digit_count(Int div ?CUT_INTEGER).


%%
-doc """
    Returns `Power`'th power of `10` (`10^Power`). This function differs from
    `math:pow/2` as it returns an integer.

    To obtain the result, this function uses `math:pow/2`, which returns a
    floating point number. Converting floats to integers might cause errors due
    to loss of precision. To combat this, `math:pow/2` is applied only to
    sufficiently small powers (less than `12`). Larger powers are calculated
    by multiplying (using integer multiplication, which is lossless) smaller
    power, that fit this interval, by `10^12` as many times as needed.

    Implementation, using string concatenation and transformation to integer
    was also considered, but it appeared to be much slower:
        erlang:list_to_integer(lists:append(["1"|lists:duplicate(Power, "0")]))

    TODO: Try recursive solution with single digit step, accumulating the result:
        fun(0,     Number) -> Number;
        fun(Power, Number) -> fun(Power-1, Number*10).
    """.
-spec ten_pow(Power :: integer()) -> TenToThePower :: integer().

ten_pow(Power) when 0 =< Power, Power <  ?CUT_INTEGER_POWER -> erlang:round(math:pow(10, Power));
ten_pow(Power) when             Power >= ?CUT_INTEGER_POWER -> ten_pow(Power-?CUT_INTEGER_POWER) * ?CUT_INTEGER.


%%
-doc """
    Returns an integer, which results from concatenating second parameter to
    the end of the first parameter
        > ja_erl_utils_int:concat(123, 456).
        123456
    """.
-spec concat(IntStart :: integer(), IntEnd :: integer()) ->
    IntStartEnd :: integer().

concat(IntStart, 0) when IntStart>0 ->
    IntStart*10;

concat(IntStart, IntEnd) when IntStart>0, IntEnd>0 ->
    Power = digit_count(IntEnd),
    IntStart * ten_pow(Power) + IntEnd.


%%
-doc """
    Splits integer `Integer` into two integers, so that the second integer
    contains `Split` last digits of the initial integer and the first integer
    contains the rest digits.

        > ja_erl_utils_int:split(12345, 2).
        {123,45}

    Returns `undefined`, if `Integer` does not contain more digits than `Split`.
    """.
-spec split(Integer :: integer(), Split :: integer()) ->
    {IntStart :: integer(), IntEnd :: integer()} | undefined.

split(Integer, Split) when Integer > 9, Split > 0 ->
    Power = ten_pow(Split),
    case Integer >= Power of
        true  -> {Integer div Power, Integer rem Power};
        false -> undefined
    end.


%%
-doc """
    Calculates the quotient of Euclidean division `A/B`.

    >#### Euclidean division {: .info }
    >According to Euclid's division lemma, for any two integers `A` and `B`,
    >such that `B =/= 0`, there is a unique pair `Q` and `R` such that
    >`A = Q*B + R` and `0 =< R < |B|`. This `Q` is the quotient of Euclidean
    >division and `R` is the remainder.

    From the lemma it follows that quotient is such integer, that gives the
    largest product `Q*B`, which is still not greater than `A`. If `B` is
    positive, then quotient is the largest integer, which is still not greater
    than the result of floating point division `A/B`. If `B` is negative, then
    quotient is the smallest integer, which is still not smaller than the
    result of floating point division `A/B`. In some cases this result differs
    from the result of Erlang's `div` operator:
        > ja_erl_utils_int:euclidean_div(-7, 3).
        -3
        2> -7 div 3.
        -2
        3> ja_erl_utils_int:euclidean_div(-7, -3).
        3
        4> -7 div -3.
        2
    """.
-spec euclidean_div(A :: integer(), B :: integer()) -> ADivB :: integer().

euclidean_div(A, B) when A>0 -> A div B;
euclidean_div(0, _)          -> 0;
euclidean_div(A, B) when A<0 ->
    case A rem B of
        0            -> A div B;
        _ when B > 0 -> A div B - 1;
        _ when B < 0 -> A div B + 1
    end.


%%
-doc """
    Calculates the remainder of Euclidean division `A/B`.

    The result is always a non negative integer, which is smaller than absolute
    value of `B`. In some cases it differs from the result of Erlang's `rem`
    operator:
        > ja_erl_utils_int:euclidean_rem(-7, 3).
        2
        > -7 rem 3.
        -1
        > ja_erl_utils_int:euclidean_rem(-7, -3).
        2
        > -7 rem -3.
        -1

    For the definition of Euclidean division, see `euclidean_div/2`.
    """.
-spec euclidean_rem(A :: integer(), B :: integer()) -> ARemB :: integer().

euclidean_rem(A, B) ->
    case A rem B of
        Pos when Pos >= 0 -> Pos;
        Neg when B > 0    -> Neg + B;
        Neg when B < 0    -> Neg - B
    end.


%%
-doc """
    Solves a single linear equation `A*x = B` for integer `x`.

    Returns a value of `x`, if it is the only integer solution of the equation.
    If there are infinitely many integer solutions (`0*x = 0`), returns `any`.
    If there are no integer solutions (`2*x = 3` or `0*x = 3`), returns `undefined`.
        > ja_erl_utils_int:solve_one_equation({2, -4}).
        -2
        > ja_erl_utils_int:solve_one_equation({0, 0}).
        any
        > ja_erl_utils_int:solve_one_equation({2, 3}).
        undefined
        > ja_erl_utils_int:solve_one_equation({0, 3}).
        undefined
    """.
-spec solve_one_equation(Equation :: {A :: integer(), B :: integer()}) ->
    integer() | any | undefined.

solve_one_equation({0, 0}) -> any;
solve_one_equation({0, _}) -> undefined;
solve_one_equation({A, B}) ->
    case B rem A of
        0 -> B div A;
        _ -> undefined
    end.


%%
-doc """
    Solves system of two linear equations with two variables for integers.

    For equation system
        A1*x + B1*y = C1
        A2*x + B2*y = C2
    returns a tuple of `x` and `y` values, if it is the only solution. If there
    are infinitely many possible integer values for `x` or `y`, returns `any`
    in that position. If `y` is dependent on `x`, returns `{any, String}`,
    where `String` is an arithmetic expression, representing `y`'s dependency
    on `x`. Note, that in such case `x` is not any integer, but any integer,
    which makes `y` value (`String` expression) integer. If the system has no
    integer solutions, returns undefined.
        > ja_erl_utils_int:solve_two_equations({3, 4, 2}, {-2, 1, -5}).
        {2,-1}
        > ja_erl_utils_int:solve_two_equations({0, 2, 4}, {0, -3, -6}).
        {any,2}
        > ja_erl_utils_int:solve_two_equations({0, 0, 0}, {1, 0, -2}).
        {-2,any}
        > ja_erl_utils_int:solve_two_equations({0, 0, 0}, {0, 0, 0}).
        {any,any}
        > ja_erl_utils_int:solve_two_equations({1, 2, 3}, {-2, -4, -6}).
        {any,"(3 - 1*x)/2"}
        > ja_erl_utils_int:solve_two_equations({1, -2, 3}, {-1, -3, -5}).
        undefined
    """.
-spec solve_two_equations(
        Equation1 :: {A1 :: integer(), B1 :: integer(), C1 :: integer()},
        Equation2 :: {A2 :: integer(), B2 :: integer(), C2 :: integer()}
    ) ->
        {
            X :: integer() | any,
            Y :: integer() | any | string()
        } | undefined.

solve_two_equations({0, B1, C1}, {A2, B2, C2}) ->
    % B1*y = C1
    case {solve_one_equation({B1, C1}), A2 =:= 0, B2 =:= 0} of
        {undefined, _, _} -> % 0*y=C1 =/= 0 or no integer solutions
            undefined;
        {any, true, _} -> % 0*y=0
            % B2*y=C2
            case solve_one_equation({B2, C2}) of
                undefined            -> undefined;  % 0*y=C2 =/= 0 or no integer solutions
                any                  -> {any, any}; % 0*y=0
                Y when is_integer(Y) -> {any, Y}    % B2 =/= 0
            end;
        {any, false, true} ->  % 0*y=0
            % A2*x=C2, A2 =/= 0
            case solve_one_equation({A2, C2}) of
                undefined            -> undefined;  % no integer solutions
                X when is_integer(X) -> {X, any}    %
            end;
        {any, false, false} ->  % 0*y=0
            % A2*x+B2*y=C2, A2 =/= 0, B2 =/= 0
            {any, lists:flatten(io_lib:format("(~p - ~p*x)/~p", [C2, A2, B2]))};
        {Y, _, _} -> % B1 =/= 0
            % A2*x=C2-B2*y
            % A2*x=C3
            C3 = C2 - B2*Y,
            case solve_one_equation({A2, C3}) of
                undefined            -> undefined;  % 0*x=C3 =/= 0 or no integer solutions
                any                  -> {any, Y};   % 0*x=0
                X when is_integer(X) -> {X, Y}      % A2 =/= 0
            end
    end;

solve_two_equations({A1, 0, C1}, {A2, B2, C2}) -> % A1 =/= 0
    case solve_two_equations({0, A1, C1}, {B2, A2, C2}) of
        undefined -> undefined;
        {Y, X}    -> {X, Y}
    end;

solve_two_equations({A1, B1, C1}, {0, B2, C2}) -> % A1 =/= 0,  % B1 =/= 0
    solve_two_equations({0, B2, C2}, {A1, B1, C1});

solve_two_equations({A1, B1, C1}, {A2, 0, C2}) -> % A1 =/= 0,  B1 =/= 0, A2 =/= 0
    solve_two_equations({A2, 0, C2}, {A1, B1, C1});

solve_two_equations({A1, B1, C1}, {A2, B2, C2}) -> % A1 =/= 0,  B1 =/= 0, A2 =/= 0, B2 =/= 0
    % (1) A1*X + B1*Y = C1 /*A2
    % (2) A2*X + B2*Y = C2 /*A1
    % (1)-(2): B3*Y = C3
    B3 = B1*A2-B2*A1,
    C3 = C1*A2-C2*A1,
    case solve_one_equation({B3, C3}) of
        undefined ->    % 0*y=C3 =/= 0 or no integer solutions
            undefined;
        any ->  % 0*y=0
            {any, lists:flatten(io_lib:format("(~p - ~p*x)/~p", [C1, A1, B1]))};
        Y ->    % B3 =/= 0
            % A1*X + B1*Y = C1
            % A1*X = C1 - B1*Y
            % A1*X = C4
            C4 = C1 - B1*Y,
            case solve_one_equation({A1, C4}) of
                undefined -> undefined;
                X         -> {X, Y}
            end
    end.
