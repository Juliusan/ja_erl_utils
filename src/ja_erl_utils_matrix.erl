-module(ja_erl_utils_matrix).
-export([print_char_matrix/2]).
-export([get_new_matrix/2, get_char_matrix/1]).
-export([matrix_index_of/2, matrix_is_valid_index/2, matrix_next_index/3, matrix_foldl/4]).
-export([direction_all/0, direction_reverse/1, direction_clockwise/1, direction_counterclockwise/1]).

-type matrix(Type) :: #{matrix_index() => Type}.
-type matrix_index() :: {Row :: integer(), Column :: integer()}.
-type matrix_direction() :: up | right | down | left.


%%
%%  Outputs matrix Matrix, which has Dimensions dimensions (rows and columns) to terminal.
%%
-spec print_char_matrix(
    Matrix     :: matrix(term()),
    Dimensions :: matrix_index()
) ->
    ok.

print_char_matrix(Matrix, {Rows, Cols}) ->
    lists:foreach(fun(Row) ->
        lists:foreach(fun(Col) ->
            io:fwrite("~s", [[maps:get({Row, Col}, Matrix)]])
        end, lists:seq(1, Cols)),
        io:fwrite("~n")
    end, lists:seq(1, Rows)).


%%
%%  Returns matrix with specified dimensions filled with Elem.
%%
-spec get_new_matrix(
        Elem       :: term(),
        Dimensions :: matrix_index()
    ) ->
        Matrix :: matrix(term()).

get_new_matrix(Element, {Rows, Cols}) ->
    lists:foldl(fun(Row, Acc) ->
        lists:foldl(fun(Col, Accc) ->
            Accc#{{Row, Col} => Element}
        end, Acc, lists:seq(1, Cols))
    end, #{}, lists:seq(1, Rows)).


%%
%%  Converts list of lines to a matrix, with each character as separate element.
%%  Line number in list ("row") is the first element of index, character's position
%%  in line ("column") is the second element. Total number of rows and columns (Dimensions)
%%  is also returned. It is expected (although not necessary for this function) that
%%  each matrix row contains the same number of elements.
%%
-spec get_char_matrix([Line :: string()]) ->
    {
        Matrix     :: matrix(char()),
        Dimensions :: matrix_index()
    }.

get_char_matrix([]) ->
    {#{}, {0, 0}};

get_char_matrix([FirstLine|_] = Lines) ->
    Cols = erlang:length(FirstLine),
    {Rows1, Matrix} = lists:foldl(fun(Line, {Row, Acc}) ->
        {Cols1, NewAcc} = lists:foldl(fun(Pos, {Col, Accc}) ->
            NewAccc = Accc#{{Row, Col} => Pos},
            {Col+1, NewAccc}
        end, {1, Acc}, Line),
        Cols = Cols1-1,
        {Row+1,NewAcc}
    end, {1, #{}}, Lines),
    {Matrix, {Rows1-1, Cols}}.


%%
%%  Returns the index (row and column) of element Elem in matrix Matrix.
%%  If Elem is not found, undefined is returned. If there are several
%%  occurrences of Elem in Matrix, it is not specified, which index is returned.
%%
-spec matrix_index_of(Elem :: ElemType, Matrix :: matrix(ElemType)) ->
        matrix_index() | undefined
    when ElemType :: term().

matrix_index_of(Elem, Matrix) ->
    matrix_index_of_it(Elem, maps:iterator(Matrix)).

matrix_index_of_it(Elem, Iterator) ->
    case maps:next(Iterator) of
        {Index, Elem, _           } -> Index;
        {_,        _, NextIterator} -> matrix_index_of_it(Elem, NextIterator);
        none                        -> undefined
    end.



%%
%%  Checks if index Index is in matrix with dimensions Dimensions.
%%
-spec matrix_is_valid_index(
    CurrentIndex :: matrix_index(),
    Dimensions   :: matrix_index()
) ->
    boolean().

matrix_is_valid_index({ Row, _Col}, {_Rows, _Cols}) when Row<1    -> false;
matrix_is_valid_index({ Row, _Col}, { Rows, _Cols}) when Row>Rows -> false;
matrix_is_valid_index({_Row,  Col}, {_Rows, _Cols}) when Col<1    -> false;
matrix_is_valid_index({_Row,  Col}, {_Rows,  Cols}) when Col>Cols -> false;
matrix_is_valid_index({_Row, _Col}, {_Rows, _Cols})               -> true.


%%
%%  Returns index of the next element of matrix, starting with index Index in
%%  direction Direction. If there is no next element in that direction (it is
%%  out of matrix bounds), undefined is returned.
%%
-spec matrix_next_index(
    CurrentIndex :: matrix_index(),
    Direction    :: matrix_direction(),
    Dimensions   :: matrix_index()
) ->
    NextIndex :: matrix_index() | undefined.

matrix_next_index(CurrentIndex, Direction, Dimensions) ->
    NextIndex = matrix_next_index_no_check(CurrentIndex, Direction),
    case matrix_is_valid_index(NextIndex, Dimensions) of
        true  -> NextIndex;
        false -> undefined
    end.


matrix_next_index_no_check({Row, Col}, up   ) -> {Row-1, Col};
matrix_next_index_no_check({Row, Col}, right) -> {Row, Col+1};
matrix_next_index_no_check({Row, Col}, down ) -> {Row+1, Col};
matrix_next_index_no_check({Row, Col}, left ) -> {Row, Col-1}.


%%
%%  Folds through elements of matrix starting from first row and first column.
%%  Each column of the row is handled consecutively before going to next row.
%%
-spec matrix_foldl(
    FoldFun    :: fun((
                          Index       :: matrix_index(),
                          Elem        :: ElemType,
                          Accumulator :: AccType
                      ) ->
                          NewAccumulator :: AccType
                  ),
    AccIn      :: AccType,
    Matrix     :: matrix(ElemType),
    Dimensions :: matrix_index()
) ->
    AccOut :: AccType
        when
            ElemType :: term(),
            AccType  :: term().

matrix_foldl(FoldFun, AccIn, Matrix, {Rows, Cols}) ->
    lists:foldl(fun(Row, Acc) ->
        lists:foldl(fun(Col, Accc) ->
            Index = {Row, Col},
            FoldFun(Index, maps:get(Index, Matrix), Accc)
        end, Acc, lists:seq(1, Cols))
    end, AccIn, lists:seq(1, Rows)).


%%
%%  Returns all possible directions.
%%
-spec direction_all() -> [matrix_direction()].

direction_all() -> [right, down, left, up].


%%
%%  Returns opposite direction.
%%
-spec direction_reverse(matrix_direction()) -> matrix_direction().

direction_reverse(right) -> left;
direction_reverse(down ) -> up;
direction_reverse(left ) -> right;
direction_reverse(up   ) -> down.


%%
%%  Returns direction after a clockwise turn from initial.
%%
-spec direction_clockwise(matrix_direction()) -> matrix_direction().

direction_clockwise(right) -> down;
direction_clockwise(down ) -> left;
direction_clockwise(left ) -> up;
direction_clockwise(up   ) -> right.


%%
%%  Returns direction after a counterclockwise turn from initial.
%%
-spec direction_counterclockwise(matrix_direction()) -> matrix_direction().

direction_counterclockwise(right) -> up;
direction_counterclockwise(down ) -> right;
direction_counterclockwise(left ) -> down;
direction_counterclockwise(up   ) -> left.
