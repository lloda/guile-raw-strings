<!-- -*- mode: markdown; coding: utf-8 -*- -->
<!-- after https://srfi.schemers.org/srfi-template.html -->

<head>
  <style>
    code {
        color:black;
        background-color:#F0F0F0;
    }
  </style>
  <style>
    red {
        color:red;
    }
  </style>
</head>

# SRFI-??? - Raw string literals

## Abstract

Raw string literals use configurable delimiters as an alternative to the escaping syntax using in the standard string literals. The goal of this SRFI is to offer a common syntax for these literals.


## Issues

* See [9] for some discussion. <red>Needs stronger rationale to go there again.</red>

* Should more than one OPEN/CLOSE pair be offered, or be configurable?

The sample implementation allows `"`/`"`,`(`/`)`, and `[`/`]`. This has the advantage that one may pick `(`/`)` when the string has `"` in it and so on, the way `'` and `"` are mixed in some languages, without having to add a delimiter. Also using matched characters and not the same might be somewhat clearer. SRFI-109 uses `{`/`}`.

* Should the literal prefix character be something different from `#R`?

C++ raw string literals use `R`. Python uses `r`. Presumably this is for ‘raw’. Lower case seems more in line with Scheme practice. It is noted that `#r` is used in s7 Scheme [7] with a different meaning.


## Rationale

The standard string literal syntax [1] uses the character `"` as delimiter, which needs to be escaped when it's part of the string. The escape character `\` must itself be escaped. Escaping can easily get out of hand when there is more than one level of escaping or quoting involved, e.g. when writing regular expressions.

With raw string literals, the delimiter can be different for each string literal. One is free to choose a delimiter that doesn't appear in the string, which removes the need for escapes entirely.

Raw string literals are present in other languages [4, 5] and in some Schemes as a non-standard syntax extension [6, 8]. SRFI-109 [2] provides an extension to the string literal syntax that can remove the need for escapes in most cases. The syntax `&{` STRING `}` proposed there is similar to the syntax `#R"` STRING `"` proposed here. SRFI-109 still needs to handle escapes, and adds its own for newlines, substitutions, named characters, and so on. A syntax with user-defined delimiters is described, `&!` DELIMITER `{` STRING `}` DELIMITER, which is again similar to the syntax `#R` DELIMITER `"` STRING `"` DELIMITER described here. It is unclear if all the special escapes are also supported in this case. Chicken Scheme [6] offers the syntax `#<<` DELIMITER NEWLINE STRING NEWLINE DELIMITER NEWLINE for ‘multiline string constants’, and a variant using `#<#` that allows for escapes and substitutions, as SRFI-190 does.

The prefix `#R` is used to avoid incompatibilities with existing syntax and to simplify implementation. This proposal could be made substantially compatible with SRFI-109 by using `{`/`}` for OPEN/CLOSE, using `&` as prefix instead of `#R`, and requiring any non-empty user defined DELIMITER to start with `!`. But the convention of using `&` for extended literals, used in SRFI-107, 108 and 109, has not been adopted generally, and those SRFIs are supported only in one implementation, while most Schemes support extended literals starting with `#`.


## Specification

A raw string literal is a sequence of characters made of the following blocks, with nothing between them.

>  `#R` DELIMITER OPEN STRING CLOSE DELIMITER

* `#R` are the two literal characters `#R`.
* OPEN is the character `"`.
* CLOSE is the character `"`.
* DELIMITER is a sequence of characters excluding control characters, whitespace, OPEN, or CLOSE. DELIMITER may be an empty sequence.
* STRING is a sequence of characters where the sequence CLOSE DELIMITER does not apear.

OPEN, CLOSE, and DELIMITER may appear freely in STRING as long as the sequence CLOSE DELIMITER does not.

The raw string literal evaluates to a string containing the sequence of characters in STRING verbatim. No escapes are supported.

### Examples

Using an empty DELIMITER:

* `#R"hello"`  ⇒ `"hello"`
* `#R"hel\lo"`  ⇒ `"hel\\lo"`

Using `-` as DELIMITER:

* `#R-"hel\"lo"-`  ⇒ `"hel\\\"lo"`
* `#R-"he-l\"lo"-`  ⇒ `"he-l\\\"lo"`
* `#R-"he-"l\"lo"-`  ⇒ `"he-\"l\\\"lo"`
* `#R-"he-"-`  ⇒ `"he-"`
* `#R-"he-""-`  ⇒ `"he-\""`
* `#R-"he-"""-`  ⇒ `"he-\"\""`

Using `***` as DELIMITER:

* `#R***"hel\"lo"***`  ⇒ `"hel\\\"lo"`
* `#R***"he***l\"lo"***`  ⇒ `"he***l\\\"lo"`
* `#R***"he***"l\"lo"***`  ⇒ `"he***\"l\\\"lo"`
* `#R***"he***"**"***`  ⇒ `"he***\"**"`
* `#R***"he***"**""***`  ⇒ `"he***\"**\""`
* `#R***"he***"***`  ⇒ `"he***"`
* `#R***"he***""***`  ⇒ `"he***\""`
* `#R***"he***"""***`  ⇒ `"he***\"\""`

A longer example with newlines, using `|` as delimiter.

    #R|"quotes " and escapes \ and newlines
       can " freely be used " here"|

⇒

    "quotes \" and escapes \\ and newlines\n   can \" freely be used \" here"

## Implementation

The sample implementation for Guile <https://github.com/lloda/guile-raw-strings> uses a reader extension.

## References

1. Revised⁵ Report on the Algorithmic Language Scheme, Feb. 1998. §6.3.5: Strings.
2. Per Bothner, SRFI-109: Extended string quasi-literals, 2013. <https://srfi.schemers.org/srfi-109/srfi-109.html>
3. Scheme registry: # lexical syntax. <https://registry.scheme.org/#hash-syntax>
4. *Raw string literal* in <https://en.cppreference.com/w/cpp/language/string_literal>
5. *raw strings* in <https://docs.python.org/3/reference/lexical_analysis.html>
6. <https://docs.racket-lang.org/axe/index.html#%28part._raw-string%29>
7. s7: A Scheme implementation. <https://ccrma.stanford.edu/software/snd/snd/s7.html>
8. Chicken Scheme: Non-standard read syntax. <https://wiki.call-cc.org/man/5/Extensions%20to%20the%20standard>
9. SchemeCrossReference: Final SRFIs and their support. <https://practical-scheme.net/wiliki/schemexref.cgi?SRFI>
9. Discussion at `srfi-discuss@srfi.schemers.org`: <https://srfi-email.schemers.org/srfi-discuss/msg/20089402/>

## Copyright

© 2022 Daniel Llorens

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice (including the next paragraph) shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
