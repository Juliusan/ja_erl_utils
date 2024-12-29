-module(ja_erl_utils_console).
-export([print/1, print/2]).
-export([wait_key_press/0]).


%%
%%  Outputs message to terminal.
%%
-spec print(Message :: string()) ->
    ok.

print(Message) -> io:fwrite(Message ++ "~n").


%%
%%  Outputs message to terminal.
%%
-spec print(
    Pattern :: string(),
    Params  :: [term()]
) ->
    ok.

print(Pattern, Params) -> io:fwrite(Pattern ++ "~n", Params).


%%
%%  Waits for <enter> key press.
%%
-spec wait_key_press() -> ok.

wait_key_press() ->
    io:fread("", ""),
    ok.
