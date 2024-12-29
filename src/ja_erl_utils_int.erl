-module(ja_erl_utils_int).
-export([integer_digit_count/1, integer_10_pow/1, concat_integers/2, split_integer/2]).
-export([euclidean_div/2, euclidean_rem/2]).
-export([solve_one_equation_int/1, solve_two_equations_int/2]).


-define(CUT_INTEGER,       1_000_000_000_000).
-define(CUT_INTEGER_POWER, 12).


%%
%%  Returns the number of digits in non negative integer.
%%  NOTE: the last case is needed, because `math:log10/1` function returns
%%  float, which has to be converted back to integer and this results in loss
%%  of precision.
%%
-spec integer_digit_count(integer()) -> integer().

integer_digit_count(0) -> 1;
integer_digit_count(Int) when 0 < Int, Int <  ?CUT_INTEGER -> erlang:floor(math:log10(Int)) + 1;
integer_digit_count(Int) when          Int >= ?CUT_INTEGER -> ?CUT_INTEGER_POWER + integer_digit_count(Int div ?CUT_INTEGER).


%%
%%  Returns Power'th power of 10 (10^Power). This function differs from
%%  `math:pow/2` as it returns an integer.
%%  NOTE: the last case is needed, because `math:pow/2` function returns float,
%%  which has to be converted back to integer and this results in loss of
%%  precision.
%%  NOTE: other implementation was considered, but it is much slower:
%%      `erlang:list_to_integer(lists:append(["1"|lists:duplicate(Power, "0")]))`
%%
-spec integer_10_pow(integer()) -> integer().

integer_10_pow(Power) when 0 =< Power, Power <  ?CUT_INTEGER_POWER -> erlang:round(math:pow(10, Power));
integer_10_pow(Power) when             Power >= ?CUT_INTEGER_POWER -> integer_10_pow(Power-?CUT_INTEGER_POWER) * ?CUT_INTEGER.


%%
%%  Returns an integer, which results from concatenating second parameter to
%%  the end of the first parameter, e.g. 123 and 456 results in 123456.
%%
-spec concat_integers(integer(), integer()) -> integer().

concat_integers(Int1, 0) when Int1>0 ->
    Int1*10;

concat_integers(Int1, Int2) when Int1>0, Int2>0 ->
    Power = integer_digit_count(Int2),
    Int1 * integer_10_pow(Power) + Int2.


%%
%%  Splits integer Int into two integers, so that the second integer contains
%%  Split last digits of the initial integer and the first integer contains the
%%  rest digits of Int. Returns `undefined`, if Int does not contain more
%%  digits than Split.
%%
-spec split_integer(Int :: integer(), Split :: integer()) ->
    {integer(), integer()} | undefined.

split_integer(Int, Split) when Int > 9, Split > 0 ->
    Power = integer_10_pow(Split),
    case Int >= Power of
        true  -> {Int div Power, Int rem Power};
        false -> undefined
    end.


%%
%%  Calculates the quotient of euclidean division A/B.
%%
-spec euclidean_div(A :: integer(), B :: integer()) -> integer().

euclidean_div(A, B) when A>0 -> A div B;
euclidean_div(0, _)          -> 0;
euclidean_div(A, B) when A<0 ->
    case A rem B of
        0            -> A div B;
        _ when B > 0 -> A div B - 1;
        _ when B < 0 -> A div B + 1
    end.


%%
%%  Calculates the remainder of euclidean division A/B.
%%
-spec euclidean_rem(A :: integer(), B :: integer()) -> integer().

euclidean_rem(A, B) ->
    case A rem B of
        Pos when Pos >= 0 -> Pos;
        Neg when B > 0    -> Neg + B;
        Neg when B < 0    -> Neg - B
    end.


%%
%%  Solves a single equation A*x=B for x in integers. If there are infinitely
%%  many solutions, returns `any`. If there are no integer solutions, returns
%%  undefined.
%%
-spec solve_one_equation_int({A :: integer(), B :: integer()}) ->
    integer() | any | undefined.

solve_one_equation_int({0, 0}) -> any;
solve_one_equation_int({0, _}) -> undefined;
solve_one_equation_int({A, B}) ->
    case B rem A of
        0 -> B div A;
        _ -> undefined
    end.


%%
%%  Solves two equation system with two variables for x and y in integers:
%%      A1*x + B1*y = C1
%%      A2*x + B2*y = C2
%%  If there are infinitely many possible values for x or y, returns `any` in
%%  that position. If y is dependent on x, returns {any, String}, where String
%%  represents y's dependency on x. If there are no integer solutions, returns
%%  undefined.
%%
-spec solve_two_equations_int(
        {A1 :: integer(), B1 :: integer(), C1 :: integer()},
        {A2 :: integer(), B2 :: integer(), C2 :: integer()}
    ) ->
        {
            X :: integer() | any,
            Y :: integer() | any | string()
        } | undefined.

solve_two_equations_int({0, B1, C1}, {A2, B2, C2}) ->
    % B1*y = C1
    case {solve_one_equation_int({B1, C1}), A2 =:= 0, B2 =:= 0} of
        {undefined, _, _} -> % 0*y=C1 =/= 0 or no integer solutions
            undefined;
        {any, true, _} -> % 0*y=0
            % B2*y=C2
            case solve_one_equation_int({B2, C2}) of
                undefined            -> undefined;  % 0*y=C2 =/= 0 or no integer solutions
                any                  -> {any, any}; % 0*y=0
                Y when is_integer(Y) -> {any, Y}    % B2 =/= 0
            end;
        {any, false, true} ->  % 0*y=0
            % A2*x=C2, A2 =/= 0
            case solve_one_equation_int({A2, C2}) of
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
            case solve_one_equation_int({A2, C3}) of
                undefined            -> undefined;  % 0*y=C3 =/= 0 or no integer solutions
                any                  -> {any, Y};   % 0*y=0
                X when is_integer(X) -> {X, Y}      % A2 =/= 0
            end
    end;

solve_two_equations_int({A1, 0, C1}, {A2, B2, C2}) -> % A1 =/= 0
    case solve_two_equations_int({0, A1, C1}, {B2, A2, C2}) of
        undefined -> undefined;
        {Y, X}    -> {X, Y}
    end;

solve_two_equations_int({A1, B1, C1}, {0, B2, C2}) -> % A1 =/= 0,  % B1 =/= 0
    solve_two_equations_int({0, B2, C2}, {A1, B1, C1});

solve_two_equations_int({A1, B1, C1}, {A2, 0, C2}) -> % A1 =/= 0,  B1 =/= 0, A2 =/= 0
    solve_two_equations_int({A2, 0, C2}, {A1, B1, C1});

solve_two_equations_int({A1, B1, C1}, {A2, B2, C2}) -> % A1 =/= 0,  B1 =/= 0, A2 =/= 0, B2 =/= 0
    % (1) A1*X + B1*Y = C1 /*A2
    % (2) A2*X + B2*Y = C2 /*A1
    % (1)-(2): B3*Y = C3
    B3 = B1*A2-B2*A1,
    C3 = C1*A2-C2*A1,
    case solve_one_equation_int({B3, C3}) of
        undefined ->    % 0*y=C3 =/= 0 or no integer solutions
            undefined;
        any ->  % 0*y=0
            {any, lists:flatten(io_lib:format("(~p - ~p*x)/~p", [C1, A1, B1]))};
        Y ->    % B3 =/= 0
            % A1*X + B1*Y = C1
            % A1*X = C1 - B1*Y
            % A1*X = C4
            C4 = C1 - B1*Y,
            case solve_one_equation_int({A1, C4}) of
                undefined -> undefined;
                X         -> {X, Y}
            end
    end.
