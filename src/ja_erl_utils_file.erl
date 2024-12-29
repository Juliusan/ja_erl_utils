-module(ja_erl_utils_file).
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
%%  Reads the whole file FileName into a single string.
%%
-spec read_file(
    FileName :: string()
) ->
    Contents :: string().

read_file(FileName) ->
    Lines = read_lines(FileName),
    LinesR = lists:reverse(Lines),
    lists:append(LinesR).


%%
%%  Reads file FileName and returns the single line of the file.
%%  Crashes if the file contains more than one line.
%%
-spec read_only_line(
    FileName :: string()
) ->
    Line :: string().

read_only_line(FileName) ->
    [Line] = read_lines(FileName),
    Line.


%%
%%  Reads file FileName and returns the single line of the file without new
%%  line in the end. Crashes if the file contains more than one line.
%%
-spec read_only_line_no_new_line(
    FileName :: string()
) ->
    Line :: string().

read_only_line_no_new_line(FileName) ->
    [Line] = read_lines_no_new_line(FileName),
    Line.


%%
%%  Reads file FileName and returns the list of lines.
%%  The returned list is in reversed order compared to input file.
%%
-spec read_lines(
    FileName :: string()
) ->
    [Line :: string()].

read_lines(FileName) -> read_lines_to_elems(FileName, fun(Line) -> Line end).


%%
%%  Reads file FileName and returns the list of lines without new line in the end.
%%  The returned list is in reversed order compared to input file.
%%
-spec read_lines_no_new_line(
    FileName :: string()
) ->
    [Line :: string()].

read_lines_no_new_line(FileName) -> read_lines_to_elems(FileName, fun ja_erl_utils_string:drop_trailing_new_line/1).


%%
%%  Reads file FileName and converts each line to a single element in the
%%  resulting list, using LineToElemFun. The returned list is in reversed order
%%  compared to input file.
%%
-spec read_lines_to_elems(
    FileName      :: string(),
    LineToElemFun :: fun((Line :: string()) -> ElemType)
) ->
    [ElemType] when
        ElemType :: term().

read_lines_to_elems(FileName, LineToElemFun) ->
    [Elems] = read_lines_to_elems(FileName, [LineToElemFun], undefined),
    Elems.


%%
%%  Reads file FileName, which consists of several parts, separated by line Separator.
%%  Each line in each part is converted to a single element in the resulting list,
%%  using LineToElemFuns. Lines in first part is converted using first fun of LineToElemFuns,
%%  lines in second part - using second fun of LineToElemFuns etc. The list of parts
%%  is returned. Each part is a list of elements in reversed order compared to lines
%%  of input file, which were used to make them. However, the parts are in the same
%%  order as in input file.
%%
-spec read_lines_to_elems(
    FileName       :: string(),
    LineToElemFuns :: [fun((Line :: string()) -> ElemType)],
    Separator      :: string()
) ->
    [[ElemType]] when
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
%%  Same as read_lines_to_elems/2, but the read line is stripped of trailing
%%  new line character before passing to LineToElemFun.
%%
-spec read_lines_no_new_line_to_elems(
    FileName      :: string(),
    LineToElemFun :: fun((Line :: string()) -> ElemType)
) ->
    [ElemType] when
        ElemType :: term().

read_lines_no_new_line_to_elems(FileName, LineToElemFun) ->
    [Elems] = read_lines_no_new_line_to_elems(FileName, [LineToElemFun], undefined),
    Elems.


%%
%%  Same as read_lines_to_elems/3, but the read line is stripped of trailing
%%  new line character before passing any of LineToElemFun. Note that Separator
%%  is compared to the whole line, including trailing new line.
%%
-spec read_lines_no_new_line_to_elems(
    FileName       :: string(),
    LineToElemFuns :: [fun((Line :: string()) -> ElemType)],
    Separator      :: string()
) ->
    [[ElemType]] when
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
%%  Reads file FileName, which consists of groups of lines, separated by line
%%  Separator. Each group of lines is converted to group using `GroupToElemFun`.
%%  Groups are returned in reversed order compared to lines of input file,
%%  which were used to make them.
%%
-spec read_line_groups(
    FileName       :: string(),
    GroupToElemFun :: fun((Lines :: [string()]) -> GroupType),
    Separator      :: string()
) ->
    [GroupType] when
        GroupType :: term().

read_line_groups(FileName, GroupToElemFun, Separator) ->
    {ok, File} = file:open(FileName, [read]),
    Result = read_line_groups(File, GroupToElemFun, Separator, [], []),
    ok = file:close(File),
    Result.

read_line_groups(File, GroupToElemFun, Separator, Group, Groups) ->
    case file:read_line(File) of
        eof ->
            [GroupToElemFun(Group) | Groups];
        {ok, Separator} ->
            read_line_groups(File, GroupToElemFun, Separator, [], [GroupToElemFun(Group) | Groups]);
        {ok, Line} ->
            read_line_groups(File, GroupToElemFun, Separator, [Line | Group], Groups)
    end.


%%
%%  Same as read_line_groups/3, but the read lines are stripped of trailing
%%  new line character before passing to GroupToElemFun.
%%
-spec read_line_groups_no_new_line(
    FileName       :: string(),
    GroupToElemFun :: fun((Lines :: [string()]) -> GroupType),
    Separator      :: string()
) ->
    [GroupType] when
        GroupType :: term().

read_line_groups_no_new_line(FileName, GroupToElemFun, Separator) ->
    read_line_groups(FileName, fun(Lines) ->
        GroupToElemFun(lists:map(fun ja_erl_utils_string:drop_trailing_new_line/1, Lines))
    end, Separator).
