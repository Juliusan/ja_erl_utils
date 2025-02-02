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

-module(ja_erl_utils_terminal).
-moduledoc """
    Module for terminal input and output functions.

    This module is probably most useful while the application is being developed.
    It expects that standard input and output device of the program is a terminal,
    which usually is the case.
    These functions allow to interfere normal program execution in order to obtain
    information useful for debugging.
    """.
-export([print/1, print/2]).
-export([wait_key_press/0]).


%%
-doc """
    Outputs message to the terminal.

    Appends a newline character to the end of the message.
    """.
-spec print(Message :: string()) ->
    ok.

print(Message) ->
    print(Message, []).


%%
-doc """
    Outputs parametrised message to the terminal.

    Appends a newline character to the end of the message.
    `Format` variable provides message format and `Params` provides the values for the parameters.
    For more information see `io:fwrite/3`.
    """.
-spec print(
    Format :: string(),
    Params :: [term()]
) ->
    ok.

print(Format, Params) ->
    io:fwrite(Format ++ "~n", Params),
    ok.


%%
-doc "Stops execution of the program, until `Enter` key is pressed.".
-spec wait_key_press() -> ok.

wait_key_press() ->
    io:fread("", ""),
    ok.
