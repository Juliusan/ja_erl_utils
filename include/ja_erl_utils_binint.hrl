-ifndef(ja_erl_utils_bit_hrl).
-define(ja_erl_utils_bit_hrl, true).

-doc "Represents one bit.".
-type bit() :: 0 | 1.

-doc """
    Represents a binary integer number.

    The list of bits starts with the least significant bit and ends with the
    most significant one.
    """.
-type binint() :: [bit()].

-endif.
