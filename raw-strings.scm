
; (c) Daniel Llorens 2017
; Reader extension for raw strings

;; This library is free software; you can redistribute it and/or modify it under
;; the terms of the GNU Lesser General Public License as published by the Free
;; Software Foundation; either version 3 of the License, or (at your option) any
;; later version.

;; Inspired by R"delimiter( raw_characters )delimiter"
;; in http://en.cppreference.com/w/cpp/language/string_literal.
;; The quotes seem unnecessary though. So I consider them part of the delimiter; you can ellide them.

(import (ice-9 rdelim) (srfi srfi-26))

(eval-when (expand load eval)
  (define delim-begin #\()
  (define delim-end #\))
  (define (reader-extension-raw-string chr port)
    (define (char-please port)
      (let ((c (read-char port)))
        (when (eof-object? c)
          (throw 'end-of-file-reading-raw-string))
        c))
    (let* ((prefix (read-delimited (string delim-begin) port 'peek)))
      (when (string-index prefix char-whitespace?)
        (throw 'bad-raw-string-delimiter prefix))
      (unless (eqv? delim-begin (read-char port))
        (throw 'raw-string-delimiter-not-found prefix))
      (let loopa ((c (char-please port)) (s '()))
        (if (eqv? delim-end c)
          (let loopb ((ss (list delim-end)) (i 0))
            (if (= i (string-length prefix))
              (list->string (reverse! s))
              (let ((c (char-please port)))
                (if (eqv? (string-ref prefix i) c)
                  (loopb (cons c ss) (+ 1 i))
                  (loopa (char-please port) (append (cons c ss) s))))))
          (loopa (char-please port) (cons c s))))))
  (read-hash-extend #\R reader-extension-raw-string))

; Tests

(define test-reader (cut reader-extension-raw-string #\R <>))
(define (test-good test arg)
  (let ((s (call-with-input-string arg test-reader)))
    (string=? test s)))
(define (test-bad tag arg)
  (catch tag
    (lambda ()
      (call-with-input-string arg test-reader)
      #f)
    (const #t)))

(test-bad 'raw-string-delimiter-not-found "xxx||")
(test-bad 'end-of-file-reading-raw-string "xxx(thest|\"ring)xx")
(test-good "thest|\"ring" "xxx(thest|\"ring)xxx")
(test-good "thestring)xxyTHENMORE" "xxx(thestring)xxyTHENMORE)xxx")
(test-good "zeroprefix" "(zeroprefix)")

#R-(This is a long string,
	full of bad stuff like \ ' " \" \n ) ( etc.
that finally ends.)-

#R"(This is a long string,
	full of bad stuff like \ ' " \" \n ) ( etc.
that finally ends.)"
