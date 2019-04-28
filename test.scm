
; (c) Daniel Llorens 2017
; Test reader extension for raw strings

;; This library is free software; you can redistribute it and/or modify it under
;; the terms of the GNU Lesser General Public License as published by the Free
;; Software Foundation; either version 3 of the License, or (at your option) any
;; later version.

(import (raw-strings) (srfi srfi-26) (srfi srfi-64))
(set! test-log-to-file #f)

(define (test-reader s)
  (call-with-input-string s
    (cute (@@ (raw-strings) reader-extension-raw-string) #\R <>)))
(define-syntax test-error-properly
  (syntax-rules ()
    ((_ tag expr)
     (begin
       (test-error tag expr)
       (let ((r (test-result-ref (test-runner-current) 'actual-error)))
         (test-equal tag (car r)))))))

(test-begin "raw-strings")
(test-error-properly 'raw-string-delimiter-not-found (test-reader "xxx||"))
(test-error-properly 'end-of-file-reading-raw-string (test-reader "xxx(thest|\"ring)xx"))
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
(test-end "raw-strings")

#R-(This is a long string,
	full of bad stuff like \ ' " \" \n ) ( etc.
that finally ends.)-

#R-"This is a long string,
	full of bad stuff like \ ' " \" \n ) ( etc.
that finally ends."-

(exit (test-runner-fail-count (test-runner-current)))
