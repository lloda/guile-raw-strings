; -*- mode: scheme; coding: utf-8 -*-
; Test reader extension for raw strings

(import (raw-strings) (srfi srfi-26) (srfi srfi-64))

(define (test-reader s)
  (call-with-input-string s
    (cute (@@ (raw-strings) reader-extension-raw-string) #\R <>)))

(test-begin "raw-strings")
(test-error 'raw-string-delimiter-not-found (test-reader "xxx||"))
(test-error 'end-of-file-reading-raw-string (test-reader "xxx(thest|\"ring)xx"))
(test-equal "thest|\"ring" (test-reader "xxx(thest|\"ring)xxx"))
(test-equal "thestring)xxyTHENMORE" (test-reader "xxx(thestring)xxyTHENMORE)xxx"))
(test-equal "zeroprefix" (test-reader "(zeroprefix)"))
(test-equal "thest|\\\"ring" #Rxxx(thest|\"ring)xxx)
(test-equal "thestring)xxyTHENMORE" #Rxxx(thestring)xxyTHENMORE)xxx)
(test-equal "zero(\\prefix" #R(zero(\prefix))
(test-equal "hello" #R"hello")
(test-equal "(hello)" #R"(hello)")
(test-equal "\"hello\"" #R("hello"))
(test-equal "(hello)" #R[(hello)])
(test-equal #R-"he-""- "he-\"")
(test-equal #R-(he-))- "he-)")
(test-equal #R-(he-)))- "he-))")
(test-equal #R-(he-()- "he-(")
(test-equal #R***"he***"**"*** "he***\"**")
(test-equal #R***"he***""*** "he***\"")

#R-(This is a long string,
	full of bad stuff like \ ' " \" \n ) ( etc.
that finally ends.)-

#R-"This is a long string,
	full of bad stuff like \ ' " \" \n ) ( etc.
that finally ends."-

(define error-count (test-runner-fail-count (test-runner-current)))
(test-end "raw-strings")
(exit error-count)
