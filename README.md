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

The open-close pair must be matched, but the delimiter must be repeated verbatim.

* `#R("hello")`  ⇒ `"hello"` —empty delimiter, open-close-pair is `()`.
* `#R"(hello)"`  ⇒ `(hello)` —since `""` is an open-close pair, this also has an empty delimiter.
* `#R]"hello"]` ⇒ `hello` —here the delimiter is `]` and the open-close pair is `""`.
* `#R["hello"]` ⇒ `"hello"` —here the delimiter is empty and the open-close pair is `[]`.

The extension should run on Guile 2.2 or later. To enable it, install `mod/raw-strings.scm` in your module path and then ``(import (raw-strings))``.

Run the test with

    $GUILE -L mod -s test.scm

I hope you find this useful.

## References

1. Revised⁵ Report on the Algorithmic Language Scheme, Feb. 1998. §6.3.5: Strings.
2. Per Bothner, SRFI-109: Extended string quasi-literals, 2013. <https://srfi.schemers.org/srfi-109/srfi-109.html>
3. Scheme registry: # lexical syntax. <https://registry.scheme.org/#hash-syntax>
4. *Raw string literal* in <https://en.cppreference.com/w/cpp/language/string_literal>
5. *raw strings* in <https://docs.python.org/3/reference/lexical_analysis.html>
6. <https://docs.racket-lang.org/axe/index.html#%28part._raw-string%29>
7. s7: A Scheme implementation. <https://ccrma.stanford.edu/software/snd/snd/s7.html>
8. Chicken Scheme: Non-standard read syntax. <https://wiki.call-cc.org/man/5/Extensions%20to%20the%20standard>

—

<b id="f1">¹</b> You can configure the open-close pairs, as well as the extension character `R`, with the variables `openc`, `closec` and `extension-char` at the top of the source. A single open-close pair seems preferable, if everyone agrees on what that should be. [↩](#a1)
