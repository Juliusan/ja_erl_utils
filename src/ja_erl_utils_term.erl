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
-module(ja_erl_utils_term).
-moduledoc """
    Module for functions, that can be used on any Erlang term.

    Some functions of this module were part of other projects. For example,
    `format/1` and `format/2` functions are based on
    [`erlang_tools_display_term_erl:to_binary/1,2`](https://github.com/Juliusan/erlang-tools/blob/0846571c616e354583dca91551a26a57b54edecc/src/erlang_tools_display_term_erl.erl#L78-L93)
    functions of [ErlangTools](https://github.com/Juliusan/erlang-tools/)
    project.
    """.
-export([format/1, format/2, memory_usage/1]).
-export_type([position/0, position_level/0, position_level_index/0, position_level_id/0]).

-define(DEFAULT_SINGLE_INDENT_LENGTH, 2).
-define(ANY_WORD_SIZE_GUARD(S), (S =:= 4 orelse S =:= 8)).



%------------------------------------------------------------------------------
% Type definitions
%------------------------------------------------------------------------------



%%
-doc """
    Position (address) inside Erlang's term.

    Position is a list of indexes for levels of term's depth. The first element
    of the list corresponds to the outermost level of the term, the second
    element - to the second level, etc. If the term doesn't have required depth
    in required places, then the position doesn't address anything in it.

    Empty position `[]` doesn't address any part of the term.

    Here are some examples of position:
    - `[2]` addresses the second element of any list or tuple and map entry
    with key `2`: atom `this` in `[a, this, b, c]` or `{a, this, b}` and
    `2 => this` in `#{1 => a, 2 => this}`.
    - `[key]` addresses map entry with key `key`: `key => this` in
    `#{a => b, key => this}`.
    - `[3, 2, key, 1]`: if term is a list or tuple, whose third element is a
    list or tuple, whose second element is a map, which has entry with 'key' as
    a key and a list or tuple as a value, then this position addresses the
    first element of the last list or tuple: atom `this` in term
    `[a, [b,c], {d, #{e => f, key => [this, g, h, i]}, j}]`.
    - `[2, [2, 4]]`: if term is a list or tuple, whose second element is a list
    or tuple, then this position addresses the second and the fourth elements of
    the latter list or tuple: atoms `this` in term `[a, {b, this, c, this, d}]`.
    - `[2, '*']`: if term is a list or tuple, whose second element is a list or
    tuple, then this position addresses all the elements of the latter list or
    tuple: atoms `this` in term `{a, [this, this, this]}`.
    - `[2, {2, 4}]`: if term is a list or tuple, whose second element is a list
    or tuple, then this position addresses elements starting with second and
    ending with fourth of the latter list or tuple: atoms `this` in term
    `[a, {b, this, this, this, c}]`.
    """.
-type position() ::
    [ position_level() ].


%%
-doc """
    Address in single level of a position.

    It can consist of one or several indexes. If it consists of several
    indexes, it addresses all the elements addressed by at least one of the
    indexes. For example, `[1, 3]` addresses first and third elements of list
    or tuple at that level. `[2, {5, 8}]` addresses second element and all the
    elements from fifth to eighth.
    """.
-type position_level() ::
    position_level_index() |
    [ position_level_index() ].


%%
-doc """
    Atomic address of element(s) in single level of a position.

    The values can be:
    - Id of single element,
    - `'*'` meaning all the elements of list, tuple or map at that level,
    - `{From, To}`   meaning all the elements starting from id `From` and ending
    with id `End` (including `End`).
    """.
-type position_level_index() ::
    position_level_id() |
    '*' |
    {
        From :: position_level_id(),
        To   :: position_level_id()
    }.


%
-doc """
    Id of element in list, tuple or map.

    The type of id depends on the structure:
    - In a list or tuple, elements are identified by integer, which denotes
    element's position in the structure. `1` is used to identify the first
    element.
    - In a map, elements (map entries) are identified by key. It can also
    be integer, if such key is part of a map. However, it can be any term as
    map keys are not limited to integers in Erlang. Note that id identifies the
    whole entry: both key and value (not only the value). Currently there is no
    way to identify key only or value only.
    """.
-type position_level_id() ::
    integer() | term().



%------------------------------------------------------------------------------
% Functions of the module
%------------------------------------------------------------------------------



%
-doc """
    Converts term to pretty formatted binary.

    Equivalent to calling `ja_erl_utils_term:format(Term, #{})`.
    """.
-spec format(
    Term :: term()
) ->
    binary().

format(Term) ->
    format(Term, #{}).


%
-doc """
    Converts term `Term` to pretty formatted binary parametrized by `Options`.

    Pretty formatting is designed with aim to be readable for humans. All the
    list entries, tuple elements and key-value pairs of map are displayed on a
    separate line with larger indentation than term, which contains the list,
    tuple or map. The exception is lists, tuples and maps, which contain
    exactly one element (the element is not displayed on a separate line) or
    none elements at all (displayed in a single line).

    Possible keys of `Options` are:

    - `single_indent_length` (default: `2`) - number of spaces, used in single
    indent level.

    - `no_expansion` (default: `[]`) - list of positions of terms in `Term`,
    that should not be expanded to separate lines. For every position in this
    list, term addressed by it will be displayed on a single line, ignoring the
    amount of nested lists, tuples and maps in the addressed term.

        If value `true` is provided instead of a list, it means that `Term`
        itself should not be expanded. Compare:

            > Term = {[first, second], #{1 => 2, 3 => 4}}.
            {[first,second],#{1 => 2,3 => 4}}
            > ja_erl_utils_terminal:print("~s", [ja_erl_utils_term:format(Term, #{no_expansion => [['*']]})]).
            {
              [first, second],
              #{1 => 2, 3 => 4}
            }
            ok
            > ja_erl_utils_terminal:print("~s", [ja_erl_utils_term:format(Term, #{no_expansion => true})]).
            {[first, second], #{1 => 2, 3 => 4}}
            ok

        Note that `no_expansion` may be a list of positions and never a single
        position not wrapped in a list. As position itself is a list, this
        results in quite strange list in a list value `[[*]]`.

    - `expand_map_keys` (default: `false`) - wether lists, tuples and maps
    should be expanded, if they are part of a key in a map. Usually keys of a
    map are quite small and although in some cases they might be expandable, it
    is usually preferable to display them in one line. E.g., if keys of a map
    are coordinates in two dimensional space (`{X, Y}` or `#{x => X, y => Y}`)
    it would be more readable if they are displayed in one line and values of
    such map might be expanded to separate lines. Compare:

            > Term = #{{1, 2} => {first, second, third}, {3, 4} => [another, entry]}.
            #{{1,2} => {first,second,third},{3,4} => [another,entry]}
            > ja_erl_utils_terminal:print("~s", [ja_erl_utils_term:format(Term, #{expand_map_keys => false})]).
            #{
              {1, 2} => {
                first,
                second,
                third
              },
              {3, 4} => [
                another,
                entry
              ]
            }
            ok
            > ja_erl_utils_terminal:print("~s", [ja_erl_utils_term:format(Term, #{expand_map_keys => true})]).
            #{
              {
                1,
                2
              } => {
                first,
                second,
                third
              },
              {
                3,
                4
              } => [
                another,
                entry
              ]
            }
            ok
    """.
-spec format(Term, Options) -> binary()
    when
        Term    :: term(),
        Options :: #{
            single_indent_length => integer(),
            no_expansion         => [ position() ] | true,
            expand_map_keys      => boolean()
        }.

format(Term, Options) ->
    format(Term, 0, Options).


%
-doc """
    Estimates how much memory (in bytes) is used to store Erlang's term `Term`.

    This function is based on [an article about memory usage](https://www.erlang.org/doc/system/memory.html)
    in Erlang's documentation, which describes `erts-8.0` system in OTP 19.0.
    As the article itself is not very precise, this function may return:
    - an integer, which means exact number of bytes as per article;
    - `{at_least, Value}` - a term consumes at least `Value` number of bytes.
    This is needed, because memory usage of large integers is not specified
    accurately;
    - `{between, From, To}` - a term consumes at least `From` and at most `To`
    number of bytes. This is needed, because memory usage of binaries,
    references, functions, etc... is specified as "`From`-`To` words".

    The returned values are always divisible by word size of the system.

    This function does not take into account:
    - memory sharing of binaries (or other types) - each sub-term is counted
      independently and then the values are added;
    - memory, consumed by atom, node, process, port or function tables;
    - memory, consumed by emulator internal data structures used for references;

    The article does not fully cover bitstrings, which are not binaries. This
    function assumes that bitstring consumes the least possible number of full
    words. E.g., if word takes 8 bytes, then 1-64 bit length bitstrings consume
    1 word (8 bytes), 65-128 bit length bitstrings - 2 words (16 bytes), etc.

    The article doesn't specify exactly, what it means by function environment.
    This function evaluates the term (list) returned by calling
    `erlang:fun_info(Term, env)` using the same rules as for any other term.

    Finally, calling `erts_debug:flat_size(Term)` or ` erts_debug:size(Term)`
    will probably get more accurate result. However, these functions are
    hidden, not documented and considered to be for internal use only.
    """.
-spec memory_usage(Term :: term()) ->
    integer() |
    {at_least, integer()} |
    {between, integer(), integer()}.

memory_usage(Term) ->
    WordSize = ja_erl_utils:word_size(),
    case memory_usage(Term, WordSize) of
        Value when is_integer(Value) -> Value * WordSize;
        {at_least, Value}            -> {at_least, Value * WordSize};
        {between, From, To}          -> {between, From * WordSize, To * WordSize}
    end.



%------------------------------------------------------------------------------
% Private functions
%------------------------------------------------------------------------------



%
format(Atom, _Indent, _Options) when is_atom(Atom) ->
    erlang:atom_to_binary(Atom);

format(Integer, _Indent, _Options) when is_integer(Integer) ->
    erlang:integer_to_binary(Integer);

format(Float, _Indent, _Options) when is_float(Float) ->
    erlang:float_to_binary(Float, [short]);

format(Pid, _Indent, _Options) when is_pid(Pid) ->
    erlang:list_to_binary(erlang:pid_to_list(Pid));

format(Ref, _Indent, _Options) when is_reference(Ref) ->
    erlang:list_to_binary(erlang:ref_to_list(Ref));

format(Port, _Indent, _Options) when is_port(Port) ->
    erlang:list_to_binary(erlang:port_to_list(Port));

format(Fun, _Indent, _Options) when is_function(Fun) ->
    erlang:list_to_binary(erlang:fun_to_list(Fun));

format(BitStr, _Indent, _Options) when is_bitstring(BitStr) ->  % this cover binaries as every binary is a bitstring
    IsPrintable = try unicode:characters_to_list(BitStr) of
        List when is_list(List) -> io_lib:printable_unicode_list(List);
        {error,      _, _}      -> false;
        {incomplete, _, _}      -> false
    catch
        _:_                     -> false
    end,
    case {erlang:bit_size(BitStr), IsPrintable} of
        {N, true} when N>0 ->
            <<"<<\"", BitStr/binary, "\">>">>;
        {BitSize, _} ->
            {Binary, RemainderBin} = case BitSize rem 8 of
                0 ->
                    {BitStr, <<>>};
                RemBitSize ->
                    BinarySize = BitSize div 8,
                    <<Bin:BinarySize/binary, RemValue:RemBitSize/integer>> = BitStr,
                    RemValueBin   = integer_to_binary(RemValue),
                    RemBitSizeBin = integer_to_binary(RemBitSize),
                    RemBin = <<", ", RemValueBin/binary, ":", RemBitSizeBin/binary>>,
                    {Bin, RemBin}
            end,
            BinaryBin = erlang:iolist_to_binary(lists:join(", ", lists:map(fun erlang:integer_to_list/1, binary_to_list(Binary)))),
            <<"<<", BinaryBin/binary, RemainderBin/binary, ">>">>
    end;

format(List, Indent, Options) when is_list(List) ->
    case {List, io_lib:printable_unicode_list(List)} of
        {[_|_], true} ->
            ListBin = unicode:characters_to_binary(List),
            <<"\"", ListBin/binary, "\"">>;
        {_, _} ->
            IndexedList = lists:zip(lists:seq(1, erlang:length(List)), List),
            format_indexed_list(IndexedList, <<"[">>, <<"]">>, fun format/3, Indent, Options)
    end;

format(Tuple, Indent, Options) when is_tuple(Tuple) ->
    IndexedList = lists:zip(lists:seq(1, erlang:size(Tuple)), erlang:tuple_to_list(Tuple)),
    format_indexed_list(IndexedList, <<"{">>, <<"}">>, fun format/3, Indent, Options);

format(Map, Indent, Options) when is_map(Map) ->
    MapEntryToBinFun = fun({Key, Value}, I, Os) ->
        KeyOs = case maps:get(expand_map_keys, Os, false) of
            true  -> Os;
            false -> Os#{no_expansion => true}
        end,
        KeyBin = format(Key, I, KeyOs),
        ValueBin = format(Value, I, Os),
        <<KeyBin/binary, " => ", ValueBin/binary>>
    end,
    IndexedList = [ {Key, {Key, Value}} || {Key, Value} <- lists:sort(maps:to_list(Map)) ],
    format_indexed_list(IndexedList, <<"#{">>, <<"}">>, MapEntryToBinFun, Indent, Options).


%
format_indexed_list(IndexedList, StartBin, EndBin, ElemToBinFun, Indent, Options) ->
    NoExpansion = maps:get(no_expansion, Options, []),
    DoExpand = case {erlang:length(IndexedList), NoExpansion} of
        {N, _   } when N =< 1 -> false;
        {_, true}             -> false;
        {_, _   }             -> true
    end,
    case DoExpand of
        true ->
            SingleIndentLength = maps:get(single_indent_length, Options, ?DEFAULT_SINGLE_INDENT_LENGTH),
            IndentThisBin = erlang:iolist_to_binary(lists:duplicate(SingleIndentLength*Indent, <<" ">>)),
            IndentNextBin = erlang:iolist_to_binary([IndentThisBin | lists:duplicate(SingleIndentLength, <<" ">>)]),
            IndexedListConverted = erlang:iolist_to_binary(lists:join(<<",\n">>, lists:map(fun({Id, Elem}) ->
                NoExpansionNextLevel = format_no_expansion_next_level(Id, NoExpansion),
                ElemBin = ElemToBinFun(Elem, Indent+1, Options#{no_expansion => NoExpansionNextLevel}),
                <<IndentNextBin/binary, ElemBin/binary>>
            end, IndexedList))),
            <<StartBin/binary, "\n", IndexedListConverted/binary, "\n", IndentThisBin/binary, EndBin/binary>>;
        false ->
            IndexedListConverted = erlang:iolist_to_binary(lists:join(<<", ">>, lists:map(fun({Id, Elem}) ->
                NoExpansionNextLevel = format_no_expansion_next_level(Id, NoExpansion),
                ElemToBinFun(Elem, Indent, Options#{no_expansion => NoExpansionNextLevel})
            end, IndexedList))),
            <<StartBin/binary, IndexedListConverted/binary, EndBin/binary>>
    end.


%
format_no_expansion_next_level(_Id, true) ->
    true;

format_no_expansion_next_level(Id, NoExpansion) ->
    format_no_expansion_next_level(Id, NoExpansion, []).

format_no_expansion_next_level(_Id, [], AccNoExpansionNextLevel) ->
    lists:reverse(AccNoExpansionNextLevel);

format_no_expansion_next_level(Id, [[Level]|OtherPaths], AccNoExpansionNextLevel) ->
    case format_id_matches_level(Id, Level) of
        true  -> true;
        false -> format_no_expansion_next_level(Id, OtherPaths, AccNoExpansionNextLevel)
    end;

format_no_expansion_next_level(Id, [[Level|OtherLevels]|OtherPaths], AccNoExpansionNextLevel) ->
    NewAccNoExpansionNextLevel = case format_id_matches_level(Id, Level) of
        true  -> [OtherLevels | AccNoExpansionNextLevel];
        false -> AccNoExpansionNextLevel
    end,
    format_no_expansion_next_level(Id, OtherPaths, NewAccNoExpansionNextLevel).


%
format_id_matches_level(_Id, []            )                           -> false;
format_id_matches_level(_Id, ['*'|_]       )                           -> true;
format_id_matches_level(Id,  [Id|_]        )                           -> true;
format_id_matches_level(Id,  [{From, To}|_]) when From =< Id, Id =< To -> true;
format_id_matches_level(Id,  [_|Others])                               -> format_id_matches_level(Id, Others);
format_id_matches_level(Id,  NotAList)                                 -> format_id_matches_level(Id, [NotAList]).


%%
%% NOTE: this function returns number of words rather than bytes as memory_usage/1.
%%
memory_usage(I, 4) when is_integer(I), -134217729 < I, I < 134217728                   -> 1;
memory_usage(I, 8) when is_integer(I), -576460752303423489 < I, I < 576460752303423488 -> 1;
memory_usage(I, S) when is_integer(I), ?ANY_WORD_SIZE_GUARD(S)                         -> {at_least, 3};
memory_usage(A, S) when is_atom(A), ?ANY_WORD_SIZE_GUARD(S)                     -> 1;  % Atom table is not considered
memory_usage(F, 4) when is_float(F)                                             -> 4;
memory_usage(F, 8) when is_float(F)                                             -> 3;
memory_usage(B, S) when is_bitstring(B), ?ANY_WORD_SIZE_GUARD(S)                -> memory_usage_sum({between, 3, 6}, memory_usage_words_in_bitstring(B, S));
memory_usage(L, S) when is_list(L), ?ANY_WORD_SIZE_GUARD(S)                     -> memory_usage_sum(1 + length(L), memory_usage_sum_list(L, S));
memory_usage(T, S) when is_tuple(T), ?ANY_WORD_SIZE_GUARD(S)                    -> memory_usage_sum(2, memory_usage_sum_list(erlang:tuple_to_list(T), S));
memory_usage(M, S) when is_map(M), ?ANY_WORD_SIZE_GUARD(S)                      -> memory_usage_map(M, S);
memory_usage(P, S) when is_pid(P), node(P) =:= node(), ?ANY_WORD_SIZE_GUARD(S)  -> 1;
memory_usage(P, 4) when is_pid(P), node(P) =/= node()                           -> 6;
memory_usage(P, 8) when is_pid(P), node(P) =/= node()                           -> 5;
memory_usage(P, S) when is_port(P), node(P) =:= node(), ?ANY_WORD_SIZE_GUARD(S) -> 1;
memory_usage(P, S) when is_port(P), node(P) =/= node(), ?ANY_WORD_SIZE_GUARD(S) -> 5;
memory_usage(R, 4) when is_reference(R), node(R) =:= node()                     -> {between, 4, 7};
memory_usage(R, 4) when is_reference(R), node(R) =/= node()                     -> {between, 7, 9};
memory_usage(R, 8) when is_reference(R), node(R) =:= node()                     -> {between, 4, 6};
memory_usage(R, 8) when is_reference(R), node(R) =/= node()                     -> {between, 6, 7};
memory_usage(F, S) when is_function(F), ?ANY_WORD_SIZE_GUARD(S)                 -> memory_usage_sum({between, 9, 13}, memory_usage_words_in_fun_env(F, S)).


%
memory_usage_words_in_bitstring(Bitstring, WordSize) ->
    BitsInBitstring = erlang:bit_size(Bitstring),
    BitsInWord = 8*WordSize,
    (BitsInBitstring + BitsInWord - 1) div BitsInWord.


%
memory_usage_words_in_fun_env(Function, WordSize) ->
    {env, Environment} = erlang:fun_info(Function, env),
    memory_usage(Environment, WordSize).


%
memory_usage_map(Map, WordSize) ->
    AddMemoryUsageFun = fun(Key, Value, Acc) ->
        MemoryUsageKey   = memory_usage(Key, WordSize),
        MemoryUsageValue = memory_usage(Value, WordSize),
        MemoryUsageEntry = memory_usage_sum(MemoryUsageKey, MemoryUsageValue),
        memory_usage_sum(MemoryUsageEntry, Acc)
    end,
    EntriesMemoryUsage = maps:fold(AddMemoryUsageFun, 0, Map),
    case maps:size(Map) of
        Size when Size =< 32 -> memory_usage_sum(5, EntriesMemoryUsage);
        Size                 -> memory_usage_sum({between, erlang:floor(Size*1.6), erlang:ceil(Size*1.8)}, EntriesMemoryUsage)
    end.


%
memory_usage_sum(Elem1, Elem2) ->
    case lists:sort([Elem1, Elem2]) of
        [I1,                I2                ] when is_integer(I1), is_integer(I2) -> I1+I2;
        [I1,                {at_least, I2}    ] when is_integer(I1)                 -> {at_least, I1+I2};
        [I1,                {between, F2, T2} ] when is_integer(I1)                 -> {between, I1+F2, I1+T2};
        [{at_least, I1},    {at_least, I2}    ]                                     -> {at_least, I1+I2};
        [{at_least, I1},    {between, F2, _T2}]                                     -> {at_least, I1+F2};
        [{between, F1, T1}, {between, F2, T2} ]                                     -> {between, F1+F2, T1+T2}
    end.


%
memory_usage_sum_list(List, WordSize) ->
    AddMemoryUsageFun = fun(Term, Acc) ->
        MemoryUsage = memory_usage(Term, WordSize),
        memory_usage_sum(MemoryUsage, Acc)
    end,
    lists:foldl(AddMemoryUsageFun, 0, List).
