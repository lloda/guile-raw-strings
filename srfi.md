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

* Should more than one OPEN/CLOSE pair be offered, or be configurable?

The reference implementation allows `"`/`"`,`(`/`)`, and `[`/`]`. This has the advantage that one may pick `(`/`)` when the string has `"` in it and so on, the way `'` and `"` are mixed in some languages, without having to add a delimiter.

* Should the literal prefix character be something different from `#R`?

C++ raw string literals use `R`. Python uses `r`. Lower case seems more in line with common Scheme practice, e.g. SRFI-4.


## Rationale

The standard string literal syntax [1] uses the character `"` as delimiter, which needs to be escaped when it's part of the string. The escape character `\` must itself be escaped. Escaping can easily get out of hand when there is more than one level of escaping or quoting involved, e.g. when writing regular expressions.

With raw string literals, the delimiter can be different for each string literal. One is free to choose a delimiter that doesn't appear in the string, which removes the need for escapes entirely.

Raw string literals are present in other languages [2, 3] and in some Schemes as a non-standard syntax extension [4]. The present proposal is based mostly on [2], where the delimiter may be choosen freely. The prefix `#R` is used to avoid incompatibilities with existing syntax and to simplify implementation. This prefix is not claimed by any previous SRFI.


## Specification

A raw string literal is a sequence of characters made of the following blocks, with nothing between them.

>  `#R` DELIMITER OPEN STRING CLOSE DELIMITER

* `#R` are the two literal characters `#R`.
* OPEN is the character `"`.
* CLOSE is the character `"`.
* DELIMITER is an arbitrary sequence of characters other than whitespace, OPEN, or CLOSE. DELIMITER may be an empty sequence.
* STRING is a sequence of characters where the sequence CLOSE DELIMITER does not apear.

OPEN, CLOSE, and DELIMITER may appear freely in STRING as long as the sequence CLOSE DELIMITER does not.

The raw string literal evaluates to the string STRING.

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

"\(^\|[^0-9]+\)\.\([[:digit:]]+\)"

## Implementation

The implementation for Guile <https://github.com/lloda/guile-raw-strings> uses a reader extension.

## References

1. Revised⁵ Report on the Algorithmic Language Scheme, Feb. 1998. §6.3.5: Strings.
2. *Raw string literal* in <https://en.cppreference.com/w/cpp/language/string_literal>
4. *raw strings* in <https://docs.python.org/3/reference/lexical_analysis.html>
3. <https://docs.racket-lang.org/axe/index.html#%28part._raw-string%29>

## Copyright

© 2022 Daniel Llorens

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice (including the next paragraph) shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
