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

-module(ja_erl_utils_string).
-moduledoc """
    Module for string related functions.

    Contains functions, which manipulate strings and parse them into other
    types.
    """.
-export([get_integer/1, get_integer_list/1, get_integer_list/2]).
-export([drop_trailing_new_line/1]).


%%
-doc """
    Reads an integer from the start of string `String` until non digit
    character is encountered. Returns the parsed integer and the remaining
    string.

    Fails with `badarg`, if `String` doesn't start with an integer.
    """.
-spec get_integer(String) -> {integer(), RemainingString}
    when
        String          :: string(),
        RemainingString :: string().

get_integer([$-|Line]) -> {Int, NewLine} = get_pos_integer(Line), {-Int, NewLine};
get_integer(Line)      ->  get_pos_integer(Line).

get_pos_integer(Line) ->
    case get_pos_integer(Line, 0) of
        {_, Line} -> error(badarg);
        Result    -> Result
    end.

get_pos_integer([Digit|Else], Acc) when $0 =< Digit, Digit =< $9 -> get_pos_integer(Else, Acc*10 + Digit - $0);
get_pos_integer(Line,         Acc)                               -> {Acc, Line}.


%%
-doc """
    Reads a list of space separated integers from string `Line`.

    Equivalent to calling `ja_erl_utils_string:get_integer_list(Line, " ")`.
    """.
-spec get_integer_list(Line) -> [integer()]
    when
        Line :: string().

get_integer_list(Line) ->
    get_integer_list(Line, " ").


%%
-doc """
    Reads a list of `Separator` separated integers from string `Line`.

    Several consecutive `Separator`s in `Line` is equivalent to one.
    """.
-spec get_integer_list(Line, Separator) -> [integer()]
    when
        Line      :: string(),
        Separator :: string().

get_integer_list("", _) ->
    [];

get_integer_list(Line, Separator) ->
    IntegerStrs = string:split(Line, Separator, all),
    lists:filtermap(fun
        ("")         -> false;
        (IntegerStr) -> {true, erlang:list_to_integer(IntegerStr)}
    end, IntegerStrs).


%%
-doc """
    Drops trailing new line character from string `Line`.

    Fails with `badarg`, if `Line` does not end with new line.
    """.
-spec drop_trailing_new_line(Line) -> string()
    when
        Line :: string().

drop_trailing_new_line(Line) -> drop_trailing_new_line(Line, "").

drop_trailing_new_line("\n",        Acc) -> lists:reverse(Acc);
drop_trailing_new_line([Char|Line], Acc) -> drop_trailing_new_line(Line, [Char|Acc]);
drop_trailing_new_line(_,           _  ) -> error(badarg).
