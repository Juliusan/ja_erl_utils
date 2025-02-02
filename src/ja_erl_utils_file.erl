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

-module(ja_erl_utils_file).
-moduledoc """
    Module for functions that read information from file and write information to file.
    Currently there are no write functions.

    In [Advent of Code](https://adventofcode.com/) input data is provided in
    files, and there are some regularly occurring patterns of those files.
    These functions try to help with reading them. Quite possibly, they will
    be useful in other cases too.
    """.
-export([
    read_file/1,
    read_only_line/1,
    read_only_line_no_new_line/1,
    read_lines/1,
    read_lines_no_new_line/1,
    read_lines_to_elems/2,
    read_lines_to_elems/3,
    read_lines_no_new_line_to_elems/2,
    read_lines_no_new_line_to_elems/3,
    read_line_groups/3,
    read_line_groups_no_new_line/3
]).


%%
-doc "Reads the whole file `FileName` into a single string.".
-spec read_file(
    FileName :: string()
) ->
    Contents :: string().

read_file(FileName) ->
    Lines = read_lines(FileName),
    LinesR = lists:reverse(Lines),
    lists:append(LinesR).


%%
-doc """
    Reads file `FileName` and returns the single line of the file.

    Crashes if the file contains more than one line.
    """.
-spec read_only_line(
    FileName :: string()
) ->
    Line :: string().

read_only_line(FileName) ->
    [Line] = read_lines(FileName),
    Line.


%%
-doc """
    Reads file `FileName` and returns the single line of the file without new
    line in the end.

    Crashes if the file contains more than one line.
    """.
-spec read_only_line_no_new_line(
    FileName :: string()
) ->
    Line :: string().

read_only_line_no_new_line(FileName) ->
    [Line] = read_lines_no_new_line(FileName),
    Line.


%%
-doc """
    Reads file `FileName` and returns the list of lines.

    The `Lines` list is in reversed order compared to the input file. In some
    cases, there is no difference if the order of the lines is reversed, so
    they can benefit from slightly better reading performance. In other cases
    function `lists:reverse/1` can be called explicitly on the result of this
    function.
    """.
-spec read_lines(
    FileName :: string()
) ->
    Lines :: [string()].

read_lines(FileName) -> read_lines_to_elems(FileName, fun(Line) -> Line end).


%%
-doc """
    Reads file `FileName` and returns the list of lines without new line in the end.

    The `Lines` list is in reversed order compared to the input file.
    For motivation see `read_lines/1`.
    """.
-spec read_lines_no_new_line(
    FileName :: string()
) ->
    Lines :: [string()].

read_lines_no_new_line(FileName) ->
    read_lines_to_elems(FileName, fun ja_erl_utils_string:drop_trailing_new_line/1).


%%
-doc """
    Reads file `FileName` and converts each line to a single element in the
    resulting list, using `LineToElemFun` function.

    The `Elems` list is in reversed order compared to the input file.
    For motivation see `read_lines/1`.
    """.
-spec read_lines_to_elems(
    FileName      :: string(),
    LineToElemFun :: fun((Line :: string()) -> Elem :: ElemType)
) ->
    Elems :: [Elem :: ElemType] when
        ElemType :: term().

read_lines_to_elems(FileName, LineToElemFun) ->
    [Elems] = read_lines_to_elems(FileName, [LineToElemFun], undefined),
    Elems.


%%
-doc """
    Reads file `FileName`, which consists of several parts, separated by line
    `Separator`, and converts each line in each part to a single element in the
    resulting list, using one of functions in `LineToElemFuns` list.

    Lines in first part are converted using first function of `LineToElemFuns`
    list, lines in second part - using second function of `LineToElemFuns`,
    etc... The list of parts is returned. Each part is a list of elements in
    reversed order compared to lines of input file, which were used to make
    them. For motivation see `read_lines/1`. However, the parts are in the same
    order as in the input file.
    """.
-spec read_lines_to_elems(
    FileName       :: string(),
    LineToElemFuns :: [fun((Line :: string()) -> Elem :: ElemType)],
    Separator      :: string()
) ->
    Parts :: [Part :: [Elem :: ElemType]] when
        ElemType :: term().

read_lines_to_elems(FileName, LineToElemFuns, Separator) ->
    {ok, File} = file:open(FileName, [read]),
    Result = read_lines_to_elems(File, LineToElemFuns, Separator, [], []),
    ok = file:close(File),
    Result.

read_lines_to_elems(_, [], _, AccParts, []) ->
    lists:reverse(AccParts);

read_lines_to_elems(File, [LineToElemFun | LineToElemFuns], Separator, AccParts, AccElems) ->
    case file:read_line(File) of
        eof ->
            lists:reverse([AccElems | AccParts]);
        {ok, Separator} ->
            read_lines_to_elems(File, LineToElemFuns, Separator, [AccElems | AccParts], []);
        {ok, Line} ->
            Elem = LineToElemFun(Line),
            read_lines_to_elems(File, [LineToElemFun | LineToElemFuns], Separator, AccParts, [Elem | AccElems])
    end.


%%
-doc """
    Same as `read_lines_to_elems/2`, but the read line is stripped of trailing
    new line character before passing to `LineToElemFun`.
    """.
-spec read_lines_no_new_line_to_elems(
    FileName      :: string(),
    LineToElemFun :: fun((Line :: string()) -> Elem :: ElemType)
) ->
    Elems :: [Elem :: ElemType] when
        ElemType :: term().

read_lines_no_new_line_to_elems(FileName, LineToElemFun) ->
    [Elems] = read_lines_no_new_line_to_elems(FileName, [LineToElemFun], undefined),
    Elems.


%%
-doc """
    Same as `read_lines_to_elems/3`, but the read line is stripped of trailing
    new line character before passing to any of `LineToElemFuns`.

    Note that `Separator` is still compared to the whole line, including
    trailing new line character.
    """.
-spec read_lines_no_new_line_to_elems(
    FileName       :: string(),
    LineToElemFuns :: [fun((Line :: string()) -> Elem :: ElemType)],
    Separator      :: string()
) ->
    Parts :: [Part :: [Elem :: ElemType]] when
        ElemType :: term().

read_lines_no_new_line_to_elems(FileName, LineToElemFuns, Separator) ->
    NewLineToElemFuns = lists:map(fun(LineToElemFun) ->
        fun(Line) ->
            LineNoNL = ja_erl_utils_string:drop_trailing_new_line(Line),
            LineToElemFun(LineNoNL)
        end
    end, LineToElemFuns),
    read_lines_to_elems(FileName, NewLineToElemFuns, Separator).


%%
-doc """
    Reads file `FileName`, which consists of groups of lines, separated by line
    `Separator`. Each group of lines is converted to element in the resulting
    list using function `GroupToElemFun`.

    The `Elems` list is in reversed order compared to groups of lines in the
    input file, which were used to make it. Function `GroupToElemFun` receives
    lines in `GroupOfLines` parameter in reversed order too. For motivation see
    `read_lines/1`.
    """.
-spec read_line_groups(
    FileName       :: string(),
    GroupToElemFun :: fun((GroupOfLines :: [Line :: string()]) -> Elem :: ElemType),
    Separator      :: string()
) ->
    Elems :: [Elem :: ElemType] when
        ElemType :: term().

read_line_groups(FileName, GroupToElemFun, Separator) ->
    {ok, File} = file:open(FileName, [read]),
    Elems = read_line_groups(File, GroupToElemFun, Separator, [], []),
    ok = file:close(File),
    Elems.

read_line_groups(File, GroupToElemFun, Separator, Group, Elems) ->
    case file:read_line(File) of
        eof ->
            [GroupToElemFun(Group) | Elems];
        {ok, Separator} ->
            read_line_groups(File, GroupToElemFun, Separator, [], [GroupToElemFun(Group) | Elems]);
        {ok, Line} ->
            read_line_groups(File, GroupToElemFun, Separator, [Line | Group], Elems)
    end.


%%
%%  Same as read_line_groups/3, but the read lines are stripped of trailing
%%  new line character before passing to GroupToElemFun.
%%
-doc """
    Same as `read_line_groups/3`, but the read lines are stripped of trailing
    new line character before passing to `GroupToElemFun`.

    Note that `Separator` is still compared to the whole line, including
    trailing new line character.
    """.
-spec read_line_groups_no_new_line(
    FileName       :: string(),
    GroupToElemFun :: fun((GroupOfLines :: [Line :: string()]) -> Elem :: ElemType),
    Separator      :: string()
) ->
    Elems :: [Elem :: ElemType] when
        ElemType :: term().

read_line_groups_no_new_line(FileName, GroupToElemFun, Separator) ->
    read_line_groups(FileName, fun(Lines) ->
        GroupToElemFun(lists:map(fun ja_erl_utils_string:drop_trailing_new_line/1, Lines))
    end, Separator).
