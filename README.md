
The file `raw-strings.scm` contains a reader extension that lets you write verbatim strings such as

    #R-(quotes " and escapes \ and newlines
               can " freely be used " here)-

where you'd normally need escapes:

    "quotes \" and escapes \\ and newlines\n           can \" freely be used \" here"

This comes in handy for docstrings, regexps, etc.

The string between the `#R` and the `(` is the 'delimiter'. The string ends with a `)` followed by the delimiter. In the example above, the delimiter is `-`. The delimiter can be empty, as in `#R(put your '\"\\)` for `"put your '\\\"\\\\"`. If you prefer something other than `(` and `)` to ‘delimit the delimiter’ you can configure this with the constants `delim-begin` and `delim-end` in `raw-strings.scm`.

To enable the extension, install `raw-strings.scm` in your module path and then ``(import (raw-strings))``, or simply paste the code in `raw-strings.scm` in your `~/.guile`.

Run the test with

    guile -L . -s test.scm

I hope you find this useful.
