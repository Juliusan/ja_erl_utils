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

-module(ja_erl_utils_list).
-moduledoc "Module for working with lists.".
-export([count_elems_sorted/2, count_elems_start/1]).
-export([
    is_strictly_increasing/1,
    is_strictly_increasing/2,
    is_increasing/1,
    is_increasing/2,
    is_strictly_decreasing/1,
    is_strictly_decreasing/2,
    is_decreasing/1,
    is_decreasing/2,
    is_steady/2,
    is_monotonic/2
]).
-export([
    transpose/1,
    diagonals_f/1,
    diagonals_b/1,
    middle/1,
    middle_single/1,
    intersection/2,
    foldl_pairs/3,
    map_sum/2,
    map_sum_foldl/3,
    filter_count/2,
    filter_count_foldl/3
]).


%%
-doc """
    Counts number of elements `Elem` in sorted in ascending order list `List`.

    In addition to the count, returns remaining list with all the `Elem`s and
    smaller elements removed.
    """.
-spec count_elems_sorted(Elem, List) -> {Count, ListRemaining}
    when
        Elem          :: ElemType,
        List          :: [ElemType],
        Count         :: integer(),
        ListRemaining :: [ElemType],
        ElemType      :: term().

count_elems_sorted(Elem, [E1|List]  ) when E1 < Elem -> count_elems_sorted(Elem, List);
count_elems_sorted(Elem, [Elem|List])                -> count_elems_start([Elem|List]);
count_elems_sorted(_,    List       )                -> {0, List}.


%%
-doc """
    Counts how many times starting element of list `List` is repeated at the
    start of the list.

    In addition to the count, returns remaining list with all the copies of
    starting element removed from the start.
    """.
-spec count_elems_start(List) -> {Count, ListRemaining}
    when
        List          :: [ElemType],
        Count         :: integer(),
        ListRemaining :: [ElemType],
        ElemType      :: term().

count_elems_start([Elem|List]) -> count_elems_start(Elem, List, 1).

count_elems_start(Elem, [Elem|List], Acc) -> count_elems_start(Elem, List, Acc+1);
count_elems_start(_,    List,        Acc) -> {Acc, List}.


%%
-doc """
    Checks if list `List` is in strictly increasing (ascending) order.

    Every element of the list must be strictly larger than the previous one.
    """.
-spec is_strictly_increasing(List :: [number()]) -> boolean().

is_strictly_increasing(List) -> is_monotonic(List, fun(Elem1, Elem2) -> Elem1 < Elem2 end).


%%
-doc """
    Checks if list `List` is in strictly increasing (ascending) order with
    consecutive elements not further apart than `Diff`.

    Every element of the list must be strictly larger than previous element by
    not more than `Diff`.
    """.
-spec is_strictly_increasing(List :: [number()], Diff :: number()) -> boolean().

is_strictly_increasing(List, Diff) -> is_monotonic(List, fun(Elem1, Elem2) -> Elem1 < Elem2 andalso Elem2 - Elem1 =< Diff end).


%%
-doc """
    Checks if list `List` is in increasing (non-decreasing) order.

    Every element of the list must be larger or equal to the previous one.
    """.
-spec is_increasing(List :: [number()]) -> boolean().

is_increasing(List) -> is_monotonic(List, fun(Elem1, Elem2) -> Elem1 =< Elem2 end).


%%
-doc """
    Checks if list `List` is in increasing (non-decreasing) order with
    consecutive elements not further apart than `Diff`.

    Every element of the list must be larger than previous element by
    not more than `Diff` or equal to it.
    """.
-spec is_increasing(List :: [number()], Diff :: number()) -> boolean().

is_increasing(List, Diff) -> is_monotonic(List, fun(Elem1, Elem2) -> Elem1 =< Elem2 andalso Elem2 - Elem1 =< Diff end).


%%
-doc """
    Checks if list `List` is in strictly decreasing (descending) order.

    Every element of the list must be strictly smaller than the previous one.
    """.
-spec is_strictly_decreasing(List :: [number()]) -> boolean().

is_strictly_decreasing(List) -> is_monotonic(List, fun(Elem1, Elem2) -> Elem1 > Elem2 end).


%%
-doc """
    Checks if list `List` is in strictly decreasing (descending) order with
    consecutive elements not further apart than `Diff`.

    Every element of the list must be strictly smaller than previous element by
    not more than `Diff`.
    """.
-spec is_strictly_decreasing(List :: [number()], Diff :: number()) -> boolean().

is_strictly_decreasing(List, Diff) -> is_monotonic(List, fun(Elem1, Elem2) -> Elem1 > Elem2 andalso Elem1 - Elem2 =< Diff end).


%%
-doc """
    Checks if list `List` is in decreasing (non-increasing) order.

    Every element of the list must be smaller or equal to the previous one.
    """.
-spec is_decreasing(List :: [number()]) -> boolean().

is_decreasing(List) -> is_monotonic(List, fun(Elem1, Elem2) -> Elem1 >= Elem2 end).


%%
-doc """
    Checks if list `List` is in decreasing (non-increasing) order with
    consecutive elements not further apart than `Diff`.

    Every element of the list must be smaller than previous element by
    not more than `Diff` or equal to it.
    """.
-spec is_decreasing(List :: [number()], Diff :: number()) -> boolean().

is_decreasing(List, Diff) -> is_monotonic(List, fun(Elem1, Elem2) -> Elem1 >= Elem2 andalso Elem1 - Elem2 =< Diff end).


%%
-doc """
    Checks if consecutive elements of list `List` are not further apart than
    `Diff`.
    """.
-spec is_steady(List :: [number()], Diff :: number()) -> boolean().

is_steady(List, Diff) -> is_monotonic(List, fun(Elem1, Elem2) -> abs(Elem1 - Elem2) =< Diff end).


%%
-doc """
    Checks if list `List` is monotonic according to `Fun`.

    Checks if for every pair of consecutive elements `Prev` and `Next` call to
    `Fun(Prev, Next)` returns true.
    """.
-spec is_monotonic(List, Fun) -> boolean()
    when
        List :: [number()],
        Fun  :: fun((number(), number()) -> boolean()).

is_monotonic([],                  _  ) -> true;
is_monotonic([_],                 _  ) -> true;
is_monotonic([Elem1, Elem2|List], Fun) ->
    case Fun(Elem1, Elem2) of
        true  -> is_monotonic([Elem2|List], Fun);
        false -> false
    end.


%%
-doc """
    Transposes a matrix `ListOfLists` represented as list of lists.

    Transposition is flipping matrix over its diagonal:
        1> ja_erl_utils_list:transpose([[1, 2],
                                        [3, 4]]).
        [[1,3],[2,4]]
    This function is defined only if each list in `ListOfLists` is of the same
    size.
    """.
-spec transpose(ListOfLists :: [List]) -> [List]
    when List :: [term()].

transpose([])          -> [];
transpose([[]|_])      -> [];
transpose(ListOfLists) -> [lists:map(fun erlang:hd/1, ListOfLists) | transpose(lists:map(fun erlang:tl/1, ListOfLists))].


%%
-doc """
    Returns a list of all the forward diagonals (of direction `/`) of matrix
    `ListOfLists` represented as list of lists.

    Diagonals are returned in reverse order (starting from the bottom right
    corner and ending with the top left corner). Moreover, each returned
    diagonal is reversed (from the bottom left element to the top right one):
        1> ja_erl_utils_list:diagonals_f([[1, 2],
                                          [3, 4]]).
        [[4],[3,2],[1]]
    This function is defined only if each list in `ListOfLists` is of the same
    size.

    See [information box](`m:ja_erl_utils_list#diagonals_b_1_info`) in the
    documentation of `diagonals_b/1`.
    """.
-spec diagonals_f(ListOfLists :: [List]) -> [List]
    when List :: [term()].


diagonals_f(ListOfLists) ->
    diagonals_f(ListOfLists, 1, []).

diagonals_f([],          _,     AccDiagonals) -> AccDiagonals;
diagonals_f(ListOfLists, Count, AccDiagonals) ->
    {Diagonal, NewListOfLists} = diagonal_f(ListOfLists, Count, [], []),
    diagonals_f(NewListOfLists, Count+1, [Diagonal|AccDiagonals]).

diagonal_f(ListOfLists,               0,     AccDiagonal, AccListOfLists) -> {AccDiagonal, lists:reverse(AccListOfLists)++ListOfLists};
diagonal_f([],                        _,     AccDiagonal, AccListOfLists) -> {AccDiagonal, lists:reverse(AccListOfLists)};
diagonal_f([[Elem]|ListOfLists],      Index, AccDiagonal, AccListOfLists) -> diagonal_f(ListOfLists, Index-1, [Elem|AccDiagonal], AccListOfLists);
diagonal_f([[Elem|List]|ListOfLists], Index, AccDiagonal, AccListOfLists) -> diagonal_f(ListOfLists, Index-1, [Elem|AccDiagonal], [List|AccListOfLists]).


%%
-doc """
    Returns a list of all the backward diagonals (of direction `\`) of a matrix
    `ListOfLists` represented as list of lists.

    Diagonals are returned starting from the bottom left corner and ending with
    the top right corner. Moreover, each returned diagonal is reversed (from
    the bottom right element to the top left one):
        1> ja_erl_utils_list:diagonals_b([[1, 2],
                                          [3, 4]]).
        [[3],[4,1],[2]]
    This function is defined only if each list in `ListOfLists` is of the same
    size.

    >#### Consider that {: .info #diagonals_b_1_info}
    >This function was created before `m:ja_erl_utils_matrix`. It's aim was to
    >ease traversing diagonals of a matrix, which had to be represented as list
    >of lists. The function is probably not that useful now, as it is more
    >effective to walk through matrix diagonals by indexing elements in
    >`t:ja_erl_utils_matrix:matrix/1`.
    """.
-spec diagonals_b(ListOfLists :: [List]) ->  [List]
    when List :: [term()].

diagonals_b(ListOfLists) ->
    ListOfListsT = lists:map(fun lists:reverse/1, ListOfLists),
    diagonals_f(ListOfListsT).


%%
-doc """
    Returns the middle element(s) of the list `List`.

    If the list contains odd number of elements, the returned list contains a
    single middle element. If however it contains even number of elements, two
    elements next to the middle of `List` is returned.
    """.
-spec middle(List :: [ElemType]) -> [ElemType]
    when ElemType :: term().

middle(List) ->
    Length = erlang:length(List),
    Half = Length div 2,
    case Length rem 2  of
        0 -> [lists:nth(Half, List), lists:nth(Half+1, List) ];
        1 -> [lists:nth(Half + 1, List)]
    end.


%%
-doc """
    Returns the single middle element of the list `List`.

    This function is designed for lists, containing odd number of elements. If
    `List` contains even number of elements, this function crashes.
    """.
-spec middle_single(List :: [ElemType]) -> ElemType
    when ElemType :: term().

middle_single(List) ->
    [Middle] = middle(List),
    Middle.


%%
-doc """
    Finds the intersection of two lists `List1` and `List2`.

    That is, it returns a list, containing those elements, which are in both
    given lists `List1` and `List2`.
    """.
-spec intersection(List1, List2) -> Intersection
    when
        List1        :: [ElemType],
        List2        :: [ElemType],
        Intersection :: [ElemType],
        ElemType     :: term().

intersection(List1, List2) ->
    [ Elem || Elem <- List1, lists:member(Elem, List2) ].


%%
-doc """
    Folds through every pair of different elements of list `List`.

    No element is paired with itself and the order of elements of a pair is not
    important. If `List` is `[1,2,3,...,n]`, then the pairs are folded in
    following order: `{1,2}, {1,3}, ..., {1,n}, {2,3}, ..., {2,n},...,
    {n-1,n}`.
    """.
-spec foldl_pairs(Fun, Acc0, List) -> Acc1
    when
        Fun      :: fun((Elem1, Elem2, AccIn) -> AccOut),
        Acc0     :: AccType,
        Acc1     :: AccType,
        AccIn    :: AccType,
        AccOut   :: AccType,
        Elem1    :: ElemType,
        Elem2    :: ElemType,
        List     :: [ElemType],
        ElemType :: term(),
        AccType  :: term().

foldl_pairs(_,   Acc0, []         ) -> Acc0;
foldl_pairs(_,   Acc0, [_]        ) -> Acc0;
foldl_pairs(Fun, Acc0, [Elem|List]) ->
    Acc1 = foldl_pairs(Fun, Acc0, Elem, List),
    foldl_pairs(Fun, Acc1, List).

foldl_pairs(_,   Acc0, _,     []          ) -> Acc0;
foldl_pairs(Fun, Acc0, Elem1, [Elem2|List]) ->
    Acc1 = Fun(Elem1, Elem2, Acc0),
    foldl_pairs(Fun, Acc1, Elem1, List).


%%
-doc """
    Maps every element of list `List` to a number using function `Fun` and adds
    those numbers.

    Calling it is equivalent to calling `lists:sum(lists:map(Fun, List))`,
    except that `List` is traversed only once.
    """.
-spec map_sum(Fun, List) -> Sum
    when
        Fun      :: fun((Elem) -> number()),
        Elem     :: ElemType,
        List     :: [ElemType],
        Sum      :: number(),
        ElemType :: term().

map_sum(MapFun, List) ->
    lists:foldl(fun(Elem, Acc) ->
        Acc + MapFun(Elem)
    end, 0, List).


%%
-doc """
    Combines `map_sum/2` and `lists:foldl/3`.

    Calling it is equivalent to calling:
        {ListMapped, Acc1} = lists:mapfoldl(Fun, Acc0, List),
        {lists:sum(ListMapped), Acc1}.
    Except that `List` is traversed only once.
    """.
-spec map_sum_foldl(Fun, Acc0, List) -> {Sum, Acc1}
    when
        Fun      :: fun((Elem, AccIn) -> {number(), AccOut}),
        Acc0     :: AccType,
        Acc1     :: AccType,
        AccIn    :: AccType,
        AccOut   :: AccType,
        Sum      :: number(),
        Elem     :: ElemType,
        List     :: [ElemType],
        ElemType :: term(),
        AccType  :: term().

map_sum_foldl(FoldFun, InitAcc, List) ->
    lists:foldl(fun(Elem, {AccSum, Acc}) ->
        {MapResult, NewAcc} = FoldFun(Elem, Acc),
        {AccSum + MapResult, NewAcc}
    end, {0, InitAcc}, List).


%%
-doc """
    Filters elements of list `List` using function (predicate) `Pred` and
    counts the remaining elements.

    Calling it is equivalent to calling
    `erlang:length(lists:filter(Pred, List))`, except that `List` is traversed
    only once.
    """.
-spec filter_count(Pred, List) -> Count
    when
        Pred     :: fun((Elem) -> boolean()),
        Elem     :: ElemType,
        List     :: [ElemType],
        Count    :: number(),
        ElemType :: term().

filter_count(FilterFun, List) ->
    map_sum(fun(Elem) ->
        case FilterFun(Elem) of
            true  -> 1;
            false -> 0
        end
    end, List).


%%
-doc """
    Combines `filter_count/2` and `lists:foldl/3`.

    Calling it is equivalent to calling:
        {ListMapped, Acc1} = lists:mapfoldl(Fun, Acc0, List),
        {erlang:length(lists:filter(fun(E) -> E end, ListMapped)), Acc1}.
    Except that `List` is traversed only once.
    """.
-spec filter_count_foldl(Fun, Acc0, List) -> {Count, Acc1}
    when
        Fun      :: fun((Elem, AccIn) -> {boolean(), AccOut}),
        Acc0     :: AccType,
        Acc1     :: AccType,
        AccIn    :: AccType,
        AccOut   :: AccType,
        Elem     :: ElemType,
        List     :: [ElemType],
        Count    :: number(),
        ElemType :: term(),
        AccType  :: term().

filter_count_foldl(FilterFun, InitAcc, List) ->
    map_sum_foldl(fun(Elem, Acc) ->
        case FilterFun(Elem, Acc) of
            {true,  NewAcc} -> {1, NewAcc};
            {false, NewAcc} -> {0, NewAcc}
        end
    end, InitAcc, List).
