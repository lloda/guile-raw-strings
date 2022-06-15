`guile-raw-strings` is a reader extension that lets you write verbatim strings such as

    #R-(quotes " and escapes \ and newlines
               can " freely be used " here)-

where you'd normally need escapes:

    "quotes \" and escapes \\ and newlines\n           can \" freely be used \" here"

This comes in handy for docstrings, regexps, etc.

The string between the `#R` and the `(` is the 'delimiter'. The string ends with a `)` followed by the delimiter. In the example above, the delimiter is `-`. The delimiter can be empty, as in `#R(put your '\"\\)` for `"put your '\\\"\\\\"`.

You can also use `[]` or `""` instead of `()` to ‘delimit the delimiter’<sup id="a1">[1](#f1)</sup>. This means that you cannot use the characters `[("` as part of the delimiter. Whitespace in the delimiter is forbidden.

* `#R(hello)`  ⇒ `hello`
* `#R"hello"`  ⇒ `hello`
* `#R[hello]`  ⇒ `hello`
* `#Rdo-not-repeat(hello)do-not-repeat`  ⇒ `hello`

The open-close pair must be matched, but the delimiter must be repeated verbatim. It's probably better to stick to one style throughout.

* `#R("hello")`  ⇒ `"hello"` —empty delimiter, open-close-pair is `()`.
* `#R"(hello)"`  ⇒ `(hello)` —in an earlier version of this, `""` wasn't an open-close pair, so this gave `hello`.
* `#R]"hello"]` ⇒ `hello` —here the delimiter is `]` and the open-close pair is `""`.
* `#R["hello"]` ⇒ `"hello"` —here the delimiter is empty and the open-close pair is `[]`.

To enable the extension, install `mod/raw-strings.scm` in your module path and then ``(import (raw-strings))``.

Run the test with

    $GUILE -L mod -s test.scm

I hope you find this useful.

## References

* *Raw string literal* in <https://en.cppreference.com/w/cpp/language/string_literal>
* <https://docs.racket-lang.org/axe/index.html#%28part._raw-string%29>
* *raw strings* in <https://docs.python.org/3/reference/lexical_analysis.html>

—

<b id="f1">¹</b> Check the comments in `raw-strings.scm` if you want to customize this list. [↩](#a1)
