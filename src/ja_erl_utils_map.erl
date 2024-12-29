-module(ja_erl_utils_map).
-export([map_reverse/1, map_max_value/1, map_min_value/1, map_map_sum/2, map_map_count/2]).


%%
%% Reverses the map su that instead of key-value pairs it stores value-key
%% pairs. This function expects that Map is injective. If however two different
%% keys point to the same value in Map, then it is not defined, which key will
%% be assigned to the value in ReversedMap.
%%
-spec map_reverse(
    Map :: #{KeyType => ValueType}
) ->
    ReversedMap :: #{ValueType => KeyType}
        when
            KeyType   :: term(),
            ValueType :: term().

map_reverse(Map) ->
    maps:fold(fun(Key, Value, Acc) ->
        Acc#{Value => Key}
    end, #{}, Map).


%%
%%  Returns maximum among the values in map.
%%
-spec map_max_value(
    Map :: #{KeyType => ValueType}
) ->
    MaxValue :: ValueType
        when
            KeyType   :: term(),
            ValueType :: term().

map_max_value(Map) ->
    case maps:next(maps:iterator(Map)) of
        {_, FirstValue, _} ->
            maps:fold(fun(_, Value, Acc) ->
                lists:max([Value, Acc])
            end, FirstValue, Map);
        none ->
            undefined
    end.


%%
%%  Returns minimum among the values in map.
%%
-spec map_min_value(
    Map :: #{KeyType => ValueType}
) ->
    MinValue :: ValueType
        when
            KeyType   :: term(),
            ValueType :: term().

map_min_value(Map) ->
    case maps:next(maps:iterator(Map)) of
        {_, FirstValue, _} ->
            maps:fold(fun(_, Value, Acc) ->
                lists:min([Value, Acc])
            end, FirstValue, Map);
        none ->
            undefined
    end.


%%
%%  Maps every map key-value pair to number and adds those numbers.
%%
-spec map_map_sum(
    MapFun :: fun((Key :: KeyType, Value :: ValueType) -> MapResult :: number()),
    Map    :: #{KeyType => ValueType}
) ->
    Sum :: number()
        when
            KeyType   :: term(),
            ValueType :: term().

map_map_sum(MapFun, Map) ->
    maps:fold(fun(Key, Value, Acc) ->
        Acc + MapFun(Key, Value)
    end, 0, Map).


%%
%%  Filters key-value pairs using FilterFun and counts the remaining ones.
%%
-spec map_map_count(
    FilterFun :: fun((Key :: KeyType, Value :: ValueType) -> boolean()),
    Map       :: #{KeyType => ValueType}
) ->
    Sum :: number()
        when
            KeyType   :: term(),
            ValueType :: term().

map_map_count(FilterFun, Map) ->
    map_map_sum(fun(Key, Value) ->
        case FilterFun(Key, Value) of
            true  -> 1;
            false -> 0
        end
    end, Map).
