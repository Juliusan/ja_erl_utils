-module(ja_erl_utils_string).
-export([get_integer/1, get_integer_list/1, get_integer_list/2]).
-export([drop_trailing_new_line/1]).


%%
%%  Reads an integer from the string until non digit character is encountered.
%%  Fails, if any other symbol is present. Returns remaining symbols in the line.
%%
-spec get_integer(string()) -> {integer(), Remaining :: string()}.

get_integer([$-|Line]) -> {Int, NewLine} = get_pos_integer(Line), {-Int, NewLine};
get_integer(Line)      ->  get_pos_integer(Line).

get_pos_integer(Line) ->
    case get_pos_integer(Line, 0) of
        {_, NewLine} = Result when Line =/= NewLine -> Result;
        _                                           -> throw("No digits found")
    end.

get_pos_integer([Digit|Else], Acc) when $0 =< Digit, Digit =< $9 -> get_pos_integer(Else, Acc*10 + Digit - $0);
get_pos_integer(Line,         Acc)                               -> {Acc, Line}.


%%
%%  Reads a list of space separated integers from Line string.
%%
-spec get_integer_list(string()) -> [integer()].

get_integer_list(Line) ->
    get_integer_list(Line, " ").


%%
%%  Reads a list of Separator separated integers from Line string.
%%
-spec get_integer_list(Line :: string(), Separator :: string()) -> [integer()].

get_integer_list("", _) ->
    [];

get_integer_list(Line, Separator) ->
    IntegerStrsOrEmpty = string:split(Line, Separator, all),
    IntegerStrs = lists:filter(fun("") -> false; (_) -> true end, IntegerStrsOrEmpty),
    lists:map(fun(IntegerStr) -> erlang:list_to_integer(IntegerStr) end, IntegerStrs).


%%
%%  Drops trailing new line character from Line. It crashes, if Line does not
%%  end with new line.
%%
-spec drop_trailing_new_line(Line :: string()) -> string().

drop_trailing_new_line("\n")          -> "";
drop_trailing_new_line([Char | Line]) -> [Char | drop_trailing_new_line(Line)].
