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

-module(ja_erl_utils_matrix).
-moduledoc """
    Module for working with matrices: tables of data.

    For a definition of matrix see `t:matrix/1`.
    """.
-export([new/2, new/3, get/2, set/3, is_valid_index/2, dimensions/1]).
-export([values/1, index_of/2, next_index/3]).
-export([direction_all/0, direction_reverse/1, direction_clockwise/1, direction_counterclockwise/1]).
-export([print_char_matrix/1, get_char_matrix/1]).
-export([iterator/1, iterator/2, next/1]).
-export([fold/4, foldf/3, foldb/3, map/2]).
-export([map_sum/2, map_sum_foldf/3, filter_count/2, filter_count_foldf/3]).
-export_type([matrix/1, index/0, direction/0, iterator/1, iteration_opts/0]).
-deprecated([
    {direction_all, 0, "Use ?MATRIX_DIRECTIONS_ALL macro of ja_erl_utils_matrix.hrl instead"}
]).

-include("ja_erl_utils_matrix.hrl").


%------------------------------------------------------------------------------
% Type definitions
%------------------------------------------------------------------------------


%%
-doc """
    A matrix is a table of data of type `Type`.

    Matrix is composed of cells organized in rows and columns. Each row has the
    same number of columns and each column has the same number of rows.
    """.
-record(matrix, {
    rows         :: integer(),          % Number of rows in the matrix
    columns      :: integer(),          % Number of columns in the matrix
    default      :: Type,               % If matrix index does not have value among `values` map, that this is its value
    values = #{} :: #{index() => Type}  % Map of matrix values addressed by index
}).
-opaque matrix(_Type) :: #matrix{}.


%%
-doc """
    Index of a matrix cell, composing of cell's row `Row` and column `Column`.

    The index is unique for each cell. Each matrix cell can be effectively
    retrieved and changed by its index. Indexing of both rows and columns
    starts at `1`.
    """.
-type index() :: {Row :: integer(), Column :: integer()}.


%%
-doc "Direction to move to the next matrix cell.".
-type direction() :: up | right | down | left.


%%
-doc "An iterator to iterate through (possibly part of) matrix.".
-record(iterator, {
    index                :: index(),                    % Index, that will be returned next
    next_index_direction :: direction(),                % Direction on the same level (row or column) to go to next cell
    next_level_index_fun :: fun((index()) -> index()),  % Function to get the first cell index of next level (row or column)
    matrix               :: matrix(_Type)               % The matrix, which is being iterated
}).
-opaque iterator(_Type) :: #iterator{} | none.


%%
-doc """
    Options that affect the order of iteration through the matrix.

    Possible values are:
    - `row_order` (default: `left`):
        - `left`: each row should be iterated from left (the beginning) to
            right (the end).
        - `right`: each row should be iterated from right to left.
    - `column_order` (default: `top`):
        - `top`: columns should be iterated from the top (the first one) to the
            bottom (the last one).
        - `bottom`: columns should be iterated from the bottom to the top.
    - `loop_order` (default: `rows`):
        - `rows`: iteration through rows: all the columns of single row should
            be iterated before moving to the next row.
        - `columns`: iteration through columns: all the rows of single column
            should be iterated before moving to the next column.
    """.
-type iteration_opts() :: #{
    row_order    => left | right,
    column_order => top | bottom,
    loop_order   => rows | columns
}.


%------------------------------------------------------------------------------
% Functions of the module
%------------------------------------------------------------------------------


%%
-doc """
    Returns a new matrix with `Rows` rows and `Columns` columns and with all
    cells filled by `undefined`.

    Calling it is equivalent to calling
    `ja_erl_utils_matrix:new(Rows, Columns, undefined).`
    """.
-spec new(Rows, Columns) -> matrix(undefined)
    when
        Rows    :: integer(),
        Columns :: integer().

new(Rows, Columns) -> new(Rows, Columns, undefined).


%%
-doc """
    Returns a new matrix with `Rows` rows and `Columns` columns and with all
    the cells filled by `Default`.
    """.
-spec new(Rows, Columns, Default) -> matrix(Type)
    when
        Rows    :: integer(),
        Columns :: integer(),
        Default :: Type,
        Type    :: any().

new(Rows, Columns, Default) when Rows > 0, Columns > 0 ->
    #matrix{
        rows    = Rows,
        columns = Columns,
        default = Default
    };

new(_, _, _) ->
    error(badarg).


%%
-doc """
    Returns value of the cell at index `Index` of matrix `Matrix`.

    Throws `badarg` error, if `Index` is not in `Matrix`.
    """.
-spec get(Index, Matrix) -> Value
    when
        Index  :: index(),
        Matrix :: matrix(Type),
        Value  :: Type,
        Type   :: any().

get(Index, Matrix) ->
    case is_valid_index(Index, Matrix) of
        true  ->
            #matrix{
                default = Default,
                values  = Values
            } = Matrix,
            maps:get(Index, Values, Default);
        false ->
            error(badarg)
    end.


%%
-doc """
    Sets value of the cell at index `Index` of matrix `Matrix` to `Value`.

    Throws `badarg` error, if `Index` is not in `Matrix`.
    """.
-spec set(Index, Matrix1, Value) -> Matrix2
    when
        Index   :: index(),
        Matrix1 :: matrix(Type),
        Matrix2 :: matrix(Type),
        Value   :: Type,
        Type    :: any().

set(Index, Matrix1, Value) ->
    case is_valid_index(Index, Matrix1) of
        true  ->
            #matrix{values = Values} = Matrix1,
            Matrix1#matrix{values = Values#{Index => Value}};
        false ->
            error(badarg)
    end.


%%
-doc """
    Returns `true` if index `Index` is in matrix `Matrix`. Otherwise returns
    `false`.

    `Index` is in `Matrix`, if both column and row part of `Index` are
    positive, row part is not larger that row count of `Matrix` and column part
    is not larger than column count of `Matrix`.
    """.
-spec is_valid_index(Index, Matrix) -> boolean()
    when
        Index  :: index(),
        Matrix :: matrix(any()).

is_valid_index({ Row, _Col}, #matrix{rows = _   }   ) when Row<1    -> false;
is_valid_index({ Row, _Col}, #matrix{rows = Rows}   ) when Row>Rows -> false;
is_valid_index({_Row,  Col}, #matrix{columns = _   }) when Col<1    -> false;
is_valid_index({_Row,  Col}, #matrix{columns = Cols}) when Col>Cols -> false;
is_valid_index({_Row, _Col}, #matrix{}              )               -> true.


%%
-doc "Returns matrix dimensions: row count `Rows` and column count `Columns`.".
-spec dimensions(Matrix) -> Dimensions
    when
        Matrix     :: matrix(any()),
        Dimensions :: {Rows, Columns},
        Rows       :: integer(),
        Columns    :: integer().

dimensions(Matrix) ->
    #matrix{
        rows    = Rows,
        columns = Columns
    } = Matrix,
    {Rows, Columns}.


%%
-doc "Returns list of all the cell values of matrix `Matrix`.".
-spec values(Matrix) -> Values
    when
        Matrix :: matrix(Type),
        Values :: [Type],
        Type   :: any().

values(Matrix) ->
    foldb(fun(_Index, Value, Values) -> [Value | Values] end, [], Matrix).


%%
-doc """
    Returns the index of cell with value `Value` in matrix `Matrix`.

    If `Value` is not found, `undefined` is returned. If there are several
    occurrences of `Value` in `Matrix`, one of the indexes is returned, but it
    is not specified, which one.
    """.
-spec index_of(Value, Matrix) -> index() | undefined
    when
        Value  :: Type,
        Matrix :: matrix(Type),
        Type   :: any().

index_of(Value, Matrix) ->
    index_of_it(Value, iterator(Matrix)).

index_of_it(Value, Iterator) ->
    case next(Iterator) of
        {Index, Value, _           } -> Index;
        {_,     _,     NextIterator} -> index_of_it(Value, NextIterator);
        none                         -> undefined
    end.


%%
-doc """
    Returns index of the next cell of matrix `Matrix`, starting with index
    `Index` in direction `Direction`.

    If there is no cell in that direction (it is out of matrix bounds),
    `undefined` is returned.
    """.
-spec next_index(Index, Direction, Matrix) -> index() | undefined
    when
        Index     :: index(),
        Direction :: direction(),
        Matrix    :: matrix(any).

next_index(Index, Direction, Matrix) ->
    NextIndex = next_index_no_check(Index, Direction),
    case is_valid_index(NextIndex, Matrix) of
        true  -> NextIndex;
        false -> undefined
    end.


next_index_no_check({Row, Col}, up   ) -> {Row-1, Col};
next_index_no_check({Row, Col}, right) -> {Row, Col+1};
next_index_no_check({Row, Col}, down ) -> {Row+1, Col};
next_index_no_check({Row, Col}, left ) -> {Row, Col-1}.


%%
-doc "Returns all possible directions.".
-spec direction_all() -> [direction()].

direction_all() -> ?MATRIX_DIRECTIONS_ALL.


%%
-doc "Returns opposite direction of `Direction`.".
-spec direction_reverse(Direction) -> direction()
    when
        Direction :: direction().

direction_reverse(right) -> left;
direction_reverse(down ) -> up;
direction_reverse(left ) -> right;
direction_reverse(up   ) -> down.


%%
-doc "Returns direction after a clockwise turn from `Direction`.".
-spec direction_clockwise(Direction) -> direction()
    when
        Direction :: direction().

direction_clockwise(right) -> down;
direction_clockwise(down ) -> left;
direction_clockwise(left ) -> up;
direction_clockwise(up   ) -> right.


%%
-doc "Returns direction after a counterclockwise turn from `Direction`.".
-spec direction_counterclockwise(Direction) -> direction()
    when
        Direction :: direction().

direction_counterclockwise(right) -> up;
direction_counterclockwise(down ) -> right;
direction_counterclockwise(left ) -> down;
direction_counterclockwise(up   ) -> left.


%%
-doc """
    Outputs matrix `Matrix` of characters to terminal.
    """.
-spec print_char_matrix(Matrix) -> ok
    when
        Matrix :: matrix(char()).

print_char_matrix(Matrix) ->
    fold(
        fun(_, Char, Line) -> [Char | Line] end,
        "",
        Matrix,
        #{
            row_order    => right,
            column_order => top,
            loop_order   => rows,
            loop_end_fun => fun(_, Line) ->
                ja_erl_utils_terminal:print(Line),
                ""
            end
        }
    ),
    ok.


%%
-doc """
    Converts list of lines `Lines` to a matrix, with each character as a
    separate cell.

    For each character, its line position in `Lines` is the row part of cell's
    index. Character's position in its line is the column part of cell's index.
    Each line in `Lines` must contain the same number of elements.
    """.
-spec get_char_matrix(Lines) -> Matrix
    when
        Lines  :: [Line :: string()],
        Matrix :: matrix(char()).

get_char_matrix([FirstLine|_] = Lines) ->
    Rows = erlang:length(Lines),
    Cols = erlang:length(FirstLine),
    EmptyMatrix = new(Rows, Cols),
    {[], Matrix} = fold(
        fun(Index, undefined, {[[Char|AccLine]|AccLines], AccMatrix}) ->
            {[AccLine|AccLines], set(Index, AccMatrix, Char)}
        end,
        {Lines, EmptyMatrix},
        EmptyMatrix,
        #{
            row_order    => left,
            column_order => top,
            loop_order   => rows,
            loop_end_fun => fun(_, {[[]|AccLines], AccMatrix}) -> {AccLines, AccMatrix} end
        }
    ),
    Matrix.


%%
-doc """
    Returns matrix `Matrix` iterator with default iteration options.

    Equivalent to calling `iterator(Matrix, #{})`.
    """.
-spec iterator(Matrix) -> Iterator
    when
        Matrix   :: matrix(Type),
        Iterator :: iterator(Type),
        Type     :: any().

iterator(Matrix) -> iterator(Matrix, #{}).


%%
-doc """
    Returns matrix `Matrix` iterator with iteration options `Opts`.

    Iterator can be used to iterate through matrix using `next/1` function.
    """.
-spec iterator(Matrix, Opts) -> Iterator
    when
        Matrix   :: matrix(Type),
        Opts     :: iteration_opts(),
        Iterator :: iterator(Type),
        Type     :: any().

iterator(Matrix, Opts) ->
    {Rows, Cols} = dimensions(Matrix),
    {ColStart, ColNext} = case maps:get(row_order, Opts, left) of
        left  -> {1,    right};
        right -> {Cols, left }
    end,
    {RowStart, RowNext} = case maps:get(column_order, Opts, top) of
        top    -> {1,    down};
        bottom -> {Rows, up  }
    end,
    {Next, NewLevelFun} = case maps:get(loop_order, Opts, rows) of
        rows    -> {ColNext, fun(Index) -> {NewRow, _} = next_index_no_check(Index, RowNext), {NewRow, ColStart} end};
        columns -> {RowNext, fun(Index) -> {_, NewCol} = next_index_no_check(Index, ColNext), {RowStart, NewCol} end}
    end,
    #iterator{
        index                = {RowStart, ColStart},
        next_index_direction = Next,
        next_level_index_fun = NewLevelFun,
        matrix               = Matrix
    }.


%%
-doc """
    Returns next index and value according to iterator `Iterator` and a new
    iterator for remaining data.

    If there are no data according to iterator, returns `none`.
        > Matrix = ja_erl_utils_matrix:get_char_matrix(["ab","cd"]),
          It = ja_erl_utils_matrix:iterator(Matrix), ok.
        ok
        > {I1, V1, It1} = ja_erl_utils_matrix:next(It), {I1, V1}.
        {{1,1},97}
        > {I2, V2, It2} = ja_erl_utils_matrix:next(It1), {I2, V2}.
        {{1,2},98}
        > {I3, V3, It3} = ja_erl_utils_matrix:next(It2), {I3, V3}.
        {{2,1},99}
        > {I4, V4, It4} = ja_erl_utils_matrix:next(It3), {I4, V4}.
        {{2,2},100}
        > ja_erl_utils_matrix:next(It4).
        none
    """.
-spec next(Iterator) -> {Index, Value, NextIterator} | none
    when
        Iterator     :: iterator(Type),
        NextIterator :: iterator(Type),
        Index        :: index(),
        Value        :: Type,
        Type         :: any().

next(none) ->
    none;

next(Iterator) ->
    #iterator{
        index                = Index,
        next_index_direction = NextIndexDirection,
        next_level_index_fun = NextLevelIndexFun,
        matrix               = Matrix
    } = Iterator,
    NextIndex = case next_index(Index, NextIndexDirection, Matrix) of
        undefined ->
            MaybeNextIndex = NextLevelIndexFun(Index),
            case is_valid_index(MaybeNextIndex, Matrix) of
                true  -> MaybeNextIndex;
                false -> none
            end;
        NI ->
            NI
    end,
    NextIterator = case NextIndex of
        none -> none;
        _    -> Iterator#iterator{index = NextIndex}
    end,
    {Index, get(Index, Matrix), NextIterator}.


%%
-doc """
    Folds through cells of matrix `Matrix` using function `Fun` and initial
    accumulator `Acc0`. Order of fold is defined in `Opts`.

    In addition to options defined in `t:iteration_opts/0`, the following
    option in `Opts` is possible:
    - `loop_end_fun`: how accumulator must be altered after traversing full row
        (if `loop_order` is `rows`) or full column (if `loop_order` is
        `columns`). The function is passed the index of the row (if
        `loop_order` is `rows`) or column (if `loop_order` is `columns`), which
        has just been traversed. By default the same unaltered accumulator is
        returned.
    """.
-spec fold(Fun, Acc0, Matrix, Opts) -> Acc1
    when
        Fun         :: fun((Index, Value, AccIn) -> AccOut),
        Acc0        :: AccType,
        Acc1        :: AccType,
        AccIn       :: AccType,
        AccOut      :: AccType,
        Matrix      :: matrix(Type),
        Value       :: Type,
        Index       :: index(),
        Opts        :: iteration_opts() | #{
            loop_end_fun => fun((RowOrColumn, AccIn) -> AccOut)
        },
        RowOrColumn :: integer(),
        AccType     :: any(),
        Type        :: any().

fold(Fun, Acc0, Matrix, Opts) ->
    LoopEndFun = maps:get(loop_end_fun, Opts, fun(_, Acc) -> Acc end),
    LoopEndIfNeededFun = case maps:get(loop_order, Opts, rows) of
        rows ->
            fun
                ({Row, _}, none,                        AccIn) -> LoopEndFun(Row, AccIn);
                ({Row, _}, #iterator{index = {Row, _}}, AccIn) -> AccIn;
                ({Row, _}, #iterator{index = {_,   _}}, AccIn) -> LoopEndFun(Row, AccIn)
            end;
        columns ->
            fun
                ({_, Col}, none,                        AccIn) -> LoopEndFun(Col, AccIn);
                ({_, Col}, #iterator{index = {_, Col}}, AccIn) -> AccIn;
                ({_, Col}, #iterator{index = {_,   _}}, AccIn) -> LoopEndFun(Col, AccIn)
            end
    end,
    fold_with_iterator(Fun, Acc0, iterator(Matrix, Opts), LoopEndIfNeededFun).

fold_with_iterator(Fun, Acc0, Iterator, LoopEndIfNeededFun) ->
    case next(Iterator) of
        {Index, Value, NextIterator} ->
            Acc1 = Fun(Index, Value, Acc0),
            Acc2 = LoopEndIfNeededFun(Index, NextIterator, Acc1),
            fold_with_iterator(Fun, Acc2, NextIterator, LoopEndIfNeededFun);
        none ->
            Acc0
    end.


%%
-doc """
    Folds forward through cells of matrix `Matrix` using function `Fun` and
    initial accumulator `Acc0`.

    The fold is started from first row and first column. Each column of the row
    is handled consecutively before going to next row.
    """.
-spec foldf(Fun, Acc0, Matrix) -> Acc1
    when
        Fun     :: fun((Index, Value, AccIn) -> AccOut),
        Acc0    :: AccType,
        Acc1    :: AccType,
        AccIn   :: AccType,
        AccOut  :: AccType,
        Matrix  :: matrix(Type),
        Index   :: index(),
        Value   :: Type,
        AccType :: any(),
        Type    :: any().

foldf(Fun, Acc0, Matrix) ->
    fold(Fun, Acc0, Matrix, #{row_order => left, column_order => top, loop_order => rows}).


-doc """
    Folds back through cells of matrix `Matrix` using function `Fun` and
    initial accumulator `Acc0`.

    The fold is started from the last row and last column. Each column of the
    row is handled consecutively before going to previous row.
    """.
-spec foldb(Fun, Acc0, Matrix) -> Acc1
    when
        Fun     :: fun((Index, Value, AccIn) -> AccOut),
        Acc0    :: AccType,
        Acc1    :: AccType,
        AccIn   :: AccType,
        AccOut  :: AccType,
        Matrix  :: matrix(Type),
        Index   :: index(),
        Value   :: Type,
        AccType :: any(),
        Type    :: any().

foldb(Fun, Acc0, Matrix) ->
    fold(Fun, Acc0, Matrix, #{row_order => right, column_order => bottom, loop_order => rows}).


%%
-doc """
    Maps every cell of matrix `Matrix` using function `Fun` and returns new
    matrix with mapped values.
    """.
-spec map(Fun, Matrix1) -> Matrix2
    when
        Fun     :: fun((Index, Value1) -> Value2),
        Matrix1 :: matrix(Type),
        Matrix2 :: matrix(Type),
        Index   :: index(),
        Value1  :: Type,
        Value2  :: Type,
        Type    :: any().

map(Fun, Matrix) ->
    #matrix{default = Default} = Matrix,
    {Rows, Columns} = dimensions(Matrix),
    NewMatrix = new(Rows, Columns, Default),
    foldf(fun(Index, Value1, AccMatrix) ->
        Value2 = Fun(Index, Value1),
        set(Index, AccMatrix, Value2)
    end, NewMatrix, Matrix).


%%
-doc """
    Maps every cell of matrix `Matrix` to a number using function `Fun` and
    adds those numbers.

    Calling it is equivalent to calling
    `lists:sum(ja_erl_utils_matrix:values(ja_erl_utils_matrix:map(Fun, Matrix)))`,
    except that `Matrix` is traversed only once.
    """.
-spec map_sum(Fun, Matrix) -> Sum
    when
        Fun    :: fun((Index, Value) -> number()),
        Matrix :: matrix(Type),
        Index  :: index(),
        Value  :: Type,
        Sum    :: number(),
        Type   :: any().

map_sum(Fun, Matrix) ->
    foldf(fun(Index, Value, Acc) ->
        Acc + Fun(Index, Value)
    end, 0, Matrix).


%%
-doc """
    Combines `map_sum/2` and `foldf/3`.

    If `Matrix` represents matrix
        1 2
        3 4
    then
        > ja_erl_utils_matrix:map_sum_foldf(
            fun({Row, Column}, Value, AccIn) -> {Row*Column*Value, AccIn + Row + Column + Value} end,
            0,
            Matrix
          ).
        {27,22}
    """.
-spec map_sum_foldf(Fun, Acc0, Matrix) -> {Sum, Acc1}
    when
        Fun     :: fun((Index, Value, AccIn) -> {number(), AccOut}),
        Acc0    :: AccType,
        Acc1    :: AccType,
        AccIn   :: AccType,
        AccOut  :: AccType,
        Matrix  :: matrix(Type),
        Sum     :: number(),
        Index   :: index(),
        Value   :: Type,
        Type    :: term(),
        AccType :: term().

map_sum_foldf(Fun, Acc0, Matrix) ->
    foldf(fun(Index, Value, {AccSum, AccIn}) ->
        {MapResult, AccOut} = Fun(Index, Value, AccIn),
        {AccSum + MapResult, AccOut}
    end, {0, Acc0}, Matrix).


%%
-doc """
    Filters cells of matrix `Matrix` using function (predicate) `Pred` and
    counts the remaining ones.

    If `Matrix` represents matrix
        1 2 3
        4 5 6
    then
        > ja_erl_utils_matrix:filter_count(
            fun({Row, Column}, Value) -> Row =:= 2 orelse Column =:= 2 orelse Value rem 3 =:= 0 end,
            Matrix
          ).
        5
    """.
-spec filter_count(Pred, Matrix) -> Count
    when
        Pred   :: fun((Index, Value) -> boolean()),
        Matrix :: matrix(Type),
        Index  :: index(),
        Value  :: Type,
        Count  :: number(),
        Type   :: any().

filter_count(Pred, Matrix) ->
    map_sum(fun(Index, Value) ->
        case Pred(Index, Value) of
            true  -> 1;
            false -> 0
        end
    end, Matrix).


%%
-doc """
    Combines `filter_count/2` and `foldf/3`.

    If `Matrix` represents matrix
        1 2 3
        4 5 6
    then
        > ja_erl_utils_matrix:filter_count_foldf(
            fun({Row, Column}, Value, Acc) -> {Value > 4, Acc + Row + Column + Value} end,
            0,
            Matrix
          ).
        {2, 42}
    """.
-spec filter_count_foldf(Pred, Acc0, Matrix) -> {Count, Acc1}
    when
        Pred     :: fun((Index, Value, AccIn) -> {boolean(), AccOut}),
        Acc0     :: AccType,
        Acc1     :: AccType,
        AccIn    :: AccType,
        AccOut   :: AccType,
        Matrix   :: matrix(Type),
        Index    :: index(),
        Value    :: Type,
        Count    :: number(),
        Type     :: any(),
        AccType  :: any().

filter_count_foldf(Pred, Acc0, Matrix) ->
    map_sum_foldf(fun(Index, Value, AccIn) ->
        case Pred(Index, Value, AccIn) of
            {true,  AccOut} -> {1, AccOut};
            {false, AccOut} -> {0, AccOut}
        end
    end, Acc0, Matrix).
