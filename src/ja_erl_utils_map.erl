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

-module(ja_erl_utils_map).
-moduledoc "Module for working with maps.".
-export([reverse/1, max_value/1, min_value/1, map_sum/2, map_sum_fold/3, filter_count/2, filter_count_fold/3]).


%%
-doc """
    Reverses the map `Map` so that instead of key-value pairs it stores
    value-key pairs.

    This function expects that `Map` is injective. If however two (or more)
    keys are associated with the same value in `Map`, then the value in
    `ReversedMap` will be associated to one of those keys, however it is not
    defined, which one.
        > ja_erl_utils_map:reverse(#{a => 1, b => 2}).
        #{1 => a,2 => b}
    """.
-spec reverse(Map) -> ReversedMap
    when
        Map         :: #{KeyType => ValueType},
        ReversedMap :: #{ValueType => KeyType},
        KeyType     :: term(),
        ValueType   :: term().

reverse(Map) ->
    maps:fold(fun(Key, Value, Acc) ->
        Acc#{Value => Key}
    end, #{}, Map).


%%
-doc """
    Returns maximum among the values in map `Map`.

    If `Map` is empty, returns `undefined`.
    """.
-spec max_value(Map) -> MaxValue | undefined
    when
        Map       :: #{KeyType => ValueType},
        MaxValue  :: ValueType,
        KeyType   :: term(),
        ValueType :: term().

max_value(Map) ->
    case maps:next(maps:iterator(Map)) of
        {_, FirstValue, _} ->
            maps:fold(fun(_, Value, Acc) ->
                lists:max([Value, Acc])
            end, FirstValue, Map);
        none ->
            undefined
    end.


%%
-doc """
    Returns minimum among the values in map `Map`.

    If `Map` is empty, returns `undefined`.
    """.
-spec min_value(Map) -> MaxValue | undefined
    when
        Map       :: #{KeyType => ValueType},
        MaxValue  :: ValueType,
        KeyType   :: term(),
        ValueType :: term().

min_value(Map) ->
    case maps:next(maps:iterator(Map)) of
        {_, FirstValue, _} ->
            maps:fold(fun(_, Value, Acc) ->
                lists:min([Value, Acc])
            end, FirstValue, Map);
        none ->
            undefined
    end.


%%
-doc """
    Maps every key-value pair of map `Map` to a number using function `Fun` and
    adds those numbers.

    Calling it is equivalent to calling `lists:sum(maps:values(maps:map(Fun, Map)))`
    except that `Map` is traversed only once.
    """.
-spec map_sum(Fun, Map) -> Sum
    when
        Fun       :: fun((Key, Value) -> number()),
        Map       :: #{KeyType => ValueType},
        Sum       :: number(),
        Key       :: KeyType,
        Value     :: ValueType,
        KeyType   :: term(),
        ValueType :: term().

map_sum(Fun, Map) ->
    maps:fold(fun(Key, Value, Acc) ->
        Acc + Fun(Key, Value)
    end, 0, Map).


%%
-doc """
    Combines `map_sum/2` and `maps:fold/3`.

        > ja_erl_utils_map:map_sum_fold(
            fun(Key, Value, AccIn) -> {Key*Value, AccIn + Key + Value} end,
            0,
            #{1 => 2, 4 => 3, 6 => 0}
          ).
        {14,16}
    """.
-spec map_sum_fold(Fun, Acc0, Map) -> {Sum, Acc1}
    when
        Fun       :: fun((Key, Value, AccIn) -> {number(), AccOut}),
        Acc0      :: AccType,
        Acc1      :: AccType,
        AccIn     :: AccType,
        AccOut    :: AccType,
        Map       :: #{KeyType => ValueType},
        Sum       :: number(),
        Key       :: KeyType,
        Value     :: ValueType,
        KeyType   :: term(),
        ValueType :: term(),
        AccType   :: term().

map_sum_fold(Fun, Acc0, Map) ->
    maps:fold(fun(Key, Value, {AccSum, Acc}) ->
        {MapResult, NewAcc} = Fun(Key, Value, Acc),
        {AccSum + MapResult, NewAcc}
    end, {0, Acc0}, Map).


%%
-doc """
    Filters key-value pairs of map `Map` using function (predicate) `Pred` and
    counts the remaining elements.

    Calling it is equivalent to calling `maps:size(maps:filter(Pred, Map))`,
    except that `Map` is traversed only once.
    """.
-spec filter_count(Pred, Map) -> Count
    when
        Pred      :: fun((Key, Value) -> boolean()),
        Map       :: #{KeyType => ValueType},
        Count     :: number(),
        Key       :: KeyType,
        Value     :: ValueType,
        KeyType   :: term(),
        ValueType :: term().

filter_count(Pred, Map) ->
    map_sum(fun(Key, Value) ->
        case Pred(Key, Value) of
            true  -> 1;
            false -> 0
        end
    end, Map).


%%
-doc """
    Combines `filter_count/2` and `maps:fold/3`.

        > ja_erl_utils_map:filter_count_fold(
            fun(Key, Value, AccIn) -> {Key > Value, AccIn + Key + Value} end,
            0,
            #{1 => 2, 4 => 3, 6 => 0}
          ).
        {2,16}
    """.
-spec filter_count_fold(Pred, Acc0, Map) -> {Count, Acc1}
    when
        Pred      :: fun((Key, Value, AccIn) -> {boolean(), AccOut}),
        Map       :: #{KeyType => ValueType},
        Acc0      :: AccType,
        Acc1      :: AccType,
        AccIn     :: AccType,
        AccOut    :: AccType,
        Count     :: number(),
        Key       :: KeyType,
        Value     :: ValueType,
        KeyType   :: term(),
        ValueType :: term().

filter_count_fold(Pred, Acc0, Map) ->
    map_sum_fold(fun(Key, Value, Acc) ->
        case Pred(Key, Value, Acc) of
            {true,  NewAcc} -> {1, NewAcc};
            {false, NewAcc} -> {0, NewAcc}
        end
    end, Acc0, Map).
