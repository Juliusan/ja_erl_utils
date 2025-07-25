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

-module(ja_erl_utils).
-moduledoc "Root module of the library. Provides general functions.".

-export([word_size/0]).


-doc """
    Returns size (in bytes) of a single word of the system.

    This is a wrapper function, which returns the same as
    `erlang:system_info(wordsize)`. It is needed because word size must be
    mocked in some tests.
    """.
-spec word_size() -> 4 | 8.

word_size() -> erlang:system_info(wordsize).
