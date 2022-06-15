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
<!-- https://srfi.schemers.org/srfi-template.html -->

# SRFI-??? - Raw string literals

## Abstract

The goal of this SRFI is to offer a common syntax for ‘raw string’ literals. Raw string literals offer configurable delimiters as an alternative for the escaping syntax using in the standard string literals.


## Issues

* Should OPEN and CLOSE be configurable. Author thinks *no*.
* Should the type character be something different from `R`. C++ raw string literals use `R`. Python uses `r`.


## Rationale

Raw string literals make it easier to write (and read) strings containing newlines, quotes `"`), or escape characters `\`, for example those used in writing regular expressions, or when quoting code. Escaping can easily get out of hand when there is more than one level of quoting involved.


## Specification

A raw string literal is a sequence of characters with the following components and nothing between them.

>  `#R` DELIMITER OPEN STRING CLOSE DELIMITER

* `#R` are the two literal characters `#R`.
* DELIMITER is an arbitrary sequence of characters other than whitespace, OPEN, or CLOSE. DELIMITER may be an empty sequence.
* OPEN is the character `"`.
* CLOSE is the character `"`.
* STRING is a sequence of characters where the sequence CLOSE DELIMITER does not apear.

OPEN, CLOSE, and DELIMITER may appear freely in STRING as long as the sequence CLOSE DELIMITER does not.

The raw string literal evaluates to the string STRING.

### Examples

Using an empty DELIMITER:

* `#R"hello"`  ⇒ `"hello"`
* `#R"hel\lo"`  ⇒ `"hel\\lo"`
* `
#R"hel
lo"
`
⇒ `"hel\nlo"` ← <red>GH doesn't show the newline in the quoted literal</red>

Using `-` as DELIMITER:

* `#R-"hel\"lo"-`  ⇒ `"hel\\\"lo"`
* `#R-"he-l\"lo"-`  ⇒ `"he-l\\\"lo"`
* `#R-"he-"l\"lo"-`  ⇒ `"he-\"l\\\"lo"`
* `#R-"he-"-`  ⇒ `"he-"`
* `#R-"he-""-`  ⇒ `"he-\""` ← <red>bug in reference impl</red>
* `#R-"he-"""-`  ⇒ `"he-\"\""`

Using `***` as DELIMITER:

* `#R***"hel\"lo"***`  ⇒ `"hel\\\"lo"`
* `#R***"he***l\"lo"***`  ⇒ `"he***l\\\"lo"`
* `#R***"he***"l\"lo"***`  ⇒ `"he***\"l\\\"lo"`
* `#R***"he***"**"***`  ⇒ `"he***\"**"` ← <red>bug in reference impl</red>
* `#R***"he***"**""***`  ⇒ `"he***\"**\""`
* `#R***"he***"***`  ⇒ `"he-\""`
* `#R***"he***""***`  ⇒ `"he-\""` ← <red>bug in reference impl</red>
* `#R***"he***"""***`  ⇒ `"he-\"\""`

## Implementation

The following implementation for Guile <https://github.com/lloda/guile-raw-strings> uses a reader extension.

## Acknoledgements

## References

* *Raw string literal* in <https://en.cppreference.com/w/cpp/language/string_literal>
* <https://docs.racket-lang.org/axe/index.html#%28part._raw-string%29>
* *raw strings* in <https://docs.python.org/3/reference/lexical_analysis.html>

## Copyright

© 2022 Daniel Llorens

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice (including the next paragraph) shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
