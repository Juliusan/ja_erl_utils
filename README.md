# My Erlang utilities

This is an OTP library, containing various utility functions for Erlang.
It consists of some functions, which should ease code development.
The functions are grouped into modules by their domains. These modules are available:
* `m:ja_erl_utils_binint` contains functions for working with binary integers.
* `m:ja_erl_utils_file` contains functions for reading and writing files.
* `m:ja_erl_utils_int` contains functions for working integer numbers.
* `m:ja_erl_utils_list` contains functions for working with lists.
* `m:ja_erl_utils_map` contains functions for working with Erlang maps.
* `m:ja_erl_utils_matrix` contains functions for working with two dimensional arrays (matrices).
* `m:ja_erl_utils_string` contains functions for working with strings as well as parsing them.
* `m:ja_erl_utils_terminal` contains functions for working with output and input from the terminal.

You can read documentation committed to the repository [here](https://rawcdn.githack.com/Juliusan/ja_erl_utils/master/doc/index.html).

This library is heavily influenced by the needs, which arose while solving [Advent of Code](https://adventofcode.com/) tasks.
If you want to check out my solutions or at least some of them, you can visit [my Advent of Code repository](https://github.com/Juliusan/adventofcode).
The `ja_erl_utils` library was created after Advent of Code 2024.

## Using source code

The library uses `make` to do its regular tasks.

To build `ja_erl_utils` use:
    $ make compile

To run all the unit tests use:
    $ make test

It is possible to run only some unit tests:
    $ make test EUNIT_ARGS="--module=ja_erl_utils_int_tests"
    $ make test EUNIT_ARGS="--generator=ja_erl_utils_int_tests:solve_two_equations_int_test_"

For more details, see `rebar3` [documentation](https://rebar3.org/docs/commands/#eunit).

To generate documentation use:
    $ make docs

To try out the functions in console use:
    $ make run
