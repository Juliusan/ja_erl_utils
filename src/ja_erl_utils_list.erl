-module(ja_erl_utils_list).
-export([count_elems_sorted/2, count_elems_start/1]).
-export([is_decreasing/2, is_increasing/2]).
-export([
    transpose/1,
    diagonals_f/1,
    diagonals_b/1,
    middle/1,
    middle_single/1,
    intersection/2,
    foldl_pairs/3,
    list_map_sum/2,
    list_filter_count/2,
    list_foldl_sum/3,
    list_foldl_count/3
]).


%%
%%  Counts number of elements Elem in sorted list List. Returns remaining list with all the Elems (and smaller elements) removed.
%%
-spec count_elems_sorted(Elem :: ElemType, List :: [ElemType]) ->
    {Count :: integer(), Remaining :: [ElemType]}
        when ElemType :: term().

count_elems_sorted(Elem, [E1|List]  ) when E1 < Elem -> count_elems_sorted(Elem, List);
count_elems_sorted(Elem, [Elem|List])                -> count_elems_start([Elem|List]);
count_elems_sorted(_,    List       )                -> {0, List}.


%%
%%  Counts how many times starting element of List is repeated at the start of the list.
%%  Returns remaining list with all the copies of starting element removed from the start.
%%
-spec count_elems_start(List :: [ElemType]) ->
    {Count :: integer(), Remaining :: [ElemType]}
        when ElemType :: term().

count_elems_start([Elem|List]) -> count_elems_start(Elem, List, 1).

count_elems_start(Elem, [Elem|List], Acc) -> count_elems_start(Elem, List, Acc+1);
count_elems_start(_,    List,        Acc) -> {Acc, List}.


%%
%%  Checks if List is in strictly descending order with consecutive elements not further
%%  apart than Diff
%%
-spec is_decreasing(List :: [number()], Diff :: number()) -> boolean().

is_decreasing([],                  _   ) -> true;
is_decreasing([_],                 _   ) -> true;
is_decreasing([Elem1, Elem2|Else], Diff) when Elem1 > Elem2, Elem1-Elem2 =< Diff -> is_decreasing([Elem2|Else], Diff);
is_decreasing(_,                   _   ) -> false.


%%
%%  Checks if List is in strictly ascending order with consecutive elements not further
%%  apart than Diff
%%
-spec is_increasing(List :: [number()], Diff :: number()) -> boolean().

is_increasing([], _) -> true;
is_increasing([_], _) -> true;
is_increasing([Elem1, Elem2|Else], Diff) when Elem2 > Elem1, Elem2-Elem1 =< Diff -> is_increasing([Elem2|Else], Diff);
is_increasing(_, _) -> false.


%%
%%  Transposes list of lists ListOfLists.
%%  This function is defined only if each list in ListOfLists is of the same size.
%%
-spec transpose(ListOfLists :: [List]) ->  ListOfLists :: [List]
    when List :: [term()].

transpose([])          -> [];
transpose([[]|_])      -> [];
transpose(ListOfLists) -> [lists:map(fun erlang:hd/1, ListOfLists) | transpose(lists:map(fun erlang:tl/1, ListOfLists))].


%%
%%  Returns all the forward diagonals (of the form /) of ListOfLists.
%%  Diagonals are returned in reverse order (starting from the bottom right corner and
%%  ending with the top left corned) and each one is reversed (from bottom left element
%%  to the top right one):
%%      diagonal_f(1 2) = [4], [3,2], [1]
%%                (3 4)
%%  This function is defined only if each list in ListOfLists is of the same size.
%%
-spec diagonals_f(ListOfLists :: [List]) ->  ListOfLists :: [List]
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
%%  Returns all the backwards diagonals (of the form \) of ListOfLists.
%%  Diagonals are returned starting from the bottom left corner and ending
%%  with the top right corned. Each of the diagonals is reversed (from bottom
%%  right element to the top left one):
%%      diagonal_f(1 2) = [3], [4,1], [2]
%%                (3 4)
%%  This function is defined only if each list in ListOfLists is of the same size.
%%
-spec diagonals_b(ListOfLists :: [List]) ->  ListOfLists :: [List]
    when List :: [term()].

diagonals_b(ListOfLists) ->
    ListOfListsT = lists:map(fun lists:reverse/1, ListOfLists),
    diagonals_f(ListOfListsT).


%%
%%  Returns the middle element(s) of the List. If the list contains odd number
%%  of elements, the returned list contains a single middle element. If however
%%  it contains even number of elements, two elements next to middle of the list
%%  is returned.
%%
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
%%  Returns the single middle element of the List. Thus, this function is
%%  designed for lists containing odd number of elements. If List contains even
%%  number of elements, this function crashes.
%%
-spec middle_single(List :: [ElemType]) -> ElemType
    when ElemType :: term().

middle_single(List) ->
    [Middle] = middle(List),
    Middle.


%%
%%  Finds the intersection of two given lists. That is, returns a list,
%%  containing those elements, which are in both given lists.
%%
-spec intersection(
        List1 :: [ElemType],
        List2 :: [ElemType]
    ) ->
        Intersection :: [ElemType]
    when ElemType :: term().

intersection(List1, List2) ->
    [ Elem || Elem <- List1, lists:member(Elem, List2) ].


%%
%%  Folds through every pair of different elements of list List. If List is
%%  [1,2,3,...,n], then the fold order ir {1,2}, {1,3}, ..., {1,n}, {2,3}, ...,
%%  {2,n},...
%%
-spec foldl_pairs(
    FoldFun         :: fun((
                            Elem1       :: ElemType,
                            Elem2       :: ElemType,
                            Accumulator :: AccType
                        ) ->
                            NewAccumulator :: AccType
                    ),
    InitAccumulator :: AccType,
    List            :: [ElemType]
) ->
    FinalAccumulator :: AccType
        when
            ElemType :: term(),
            AccType  :: term().

foldl_pairs(_, AccIn, [])  -> AccIn;
foldl_pairs(_, AccIn, [_]) -> AccIn;
foldl_pairs(FoldFun, AccIn, [Elem|List]) ->
    AccOut = foldl_pairs(FoldFun, AccIn, Elem, List),
    foldl_pairs(FoldFun, AccOut, List).

foldl_pairs(_, AccIn, _, []) -> AccIn;
foldl_pairs(FoldFun, AccIn, Elem1, [Elem2|List]) ->
    AccOut = FoldFun(Elem1, Elem2, AccIn),
    foldl_pairs(FoldFun, AccOut, Elem1, List).


%%
%%  Maps every list element to number and adds those numbers.
%%
-spec list_map_sum(
    MapFun :: fun((Elem :: ElemType) -> MapResult :: number()),
    List   :: [ElemType]
) ->
    Sum :: number()
        when
            ElemType :: term().

list_map_sum(MapFun, List) ->
    lists:foldl(fun(Elem, Acc) ->
        Acc + MapFun(Elem)
    end, 0, List).


%%
%%  Filters list elements using FilterFun and counts the remaining ones.
%%
-spec list_filter_count(
    FilterFun :: fun((Elem :: ElemType) -> boolean()),
    List      :: [ElemType]
) ->
    Count :: number()
        when
            ElemType :: term().

list_filter_count(FilterFun, List) ->
    list_map_sum(fun(Elem) ->
        case FilterFun(Elem) of
            true  -> 1;
            false -> 0
        end
    end, List).


%%
%%  In addition to lists:foldl/3 maps every list element to number and adds
%%  those numbers.
%%
-spec list_foldl_sum(
    FoldFun         :: fun((
                            Elem        :: ElemType,
                            Accumulator :: AccType
                        ) -> {
                            MapResult      :: number(),
                            NewAccumulator :: AccType
                        }
                    ),
    InitAccumulator :: AccType,
    List            :: [ElemType]
) ->
    {Sum :: number(), FinalAccumulator :: AccType}
        when
            ElemType :: term(),
            AccType  :: term().

list_foldl_sum(FoldFun, InitAcc, List) ->
    lists:foldl(fun(Elem, {AccSum, Acc}) ->
        {MapResult, NewAcc} = FoldFun(Elem, Acc),
        {AccSum + MapResult, NewAcc}
    end, {0, InitAcc}, List).


%%
%%  In addition to lists:foldl/3 filters list elements using FoldFun and counts
%%  the remaining ones.
%%
-spec list_foldl_count(
    FoldFun         :: fun((
                            Elem        :: ElemType,
                            Accumulator :: AccType
                        ) -> {
                            CountElement   :: boolean(),
                            NewAccumulator :: AccType
                        }
                    ),
    InitAccumulator :: AccType,
    List            :: [ElemType]
) ->
    {Count :: number(), FinalAccumulator :: AccType}
        when
            ElemType :: term(),
            AccType  :: term().

list_foldl_count(FilterFun, InitAcc, List) ->
    list_foldl_sum(fun(Elem, Acc) ->
        case FilterFun(Elem, Acc) of
            {true,  NewAcc} -> {1, NewAcc};
            {false, NewAcc} -> {0, NewAcc}
        end
    end, InitAcc, List).
