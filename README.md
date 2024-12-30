# My Erlang utilities

This is an OTP library, containing various utility functions for Erlang.
It consists of some functions, which should ease code development.
The functions are grouped into modules by their domains. These modules are available:
* [`ja_erl_utils_bit`](ja_erl_utils_bit.html) contains functions for working with binary numbers.
* [`ja_erl_utils_console`](ja_erl_utils_console.html) contains functions for working with console output and input.
* [`ja_erl_utils_file`](ja_erl_utils_file.html) contains functions for reading and writing files.
* [`ja_erl_utils_int`](ja_erl_utils_int.html) contains functions for working integer numbers.
* [`ja_erl_utils_list`](ja_erl_utils_list.html) contains functions for working with lists.
* [`ja_erl_utils_map`](ja_erl_utils_map.html) contains functions for working with Erlang maps.
* [`ja_erl_utils_matrix`](ja_erl_utils_matrix.html) contains functions for working with two dimensional arrays (matrices).
* [`ja_erl_utils_string`](ja_erl_utils_string.html) contains functions for working with strings as well as parsing them.

This library is heavily influenced by the needs, which arose while solving [Advent of Code](https://adventofcode.com/) tasks.
If you want to check out my solutions to at least some of them, you can visit [my Advent of Code repository](https://github.com/Juliusan/adventofcode).
The `ja_erl_utils` library was created after Advent of Code 2024.

## Using source code

The library uses `make` to do its regular tasks.

To build `ja_erl_utils` use:
    $ make compile

To run all the unit tests use:
    $ make test

It is possible to run only some tests:
    $ make test EUNIT_ARGS="--module=ja_erl_utils_int_tests"
    $ make test EUNIT_ARGS="--generator=ja_erl_utils_int_tests:solve_two_equations_int_test_"

For more details, see `rebar3` [documentation](https://rebar3.org/docs/commands/#eunit).

To generate documentation use:
    $ make docs

To try out the functions in console use:
    $ make run
