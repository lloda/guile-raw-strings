
; (c) Daniel Llorens 2017
; Reader extension for raw strings

;; This library is free software; you can redistribute it and/or modify it under
;; the terms of the GNU Lesser General Public License as published by the Free
;; Software Foundation; either version 3 of the License, or (at your option) any
;; later version.

;; After R"delimiter( raw_characters )delimiter"
;; in http://en.cppreference.com/w/cpp/language/string_literal.
;; The quotes seem unnecessary, so I consider them part of the delimiter; you can ellide them.

(define-module (raw-strings)
  #:use-module ((ice-9 rdelim)))

(define delim-begin #\()
(define delim-end #\))

(define (reader-extension-raw-string chr port)
  (define (char-please port)
    (let ((c (read-char port)))
      (when (eof-object? c)
        (throw 'end-of-file-reading-raw-string))
      c))
  (let ((prefix (read-delimited (string delim-begin) port 'peek)))
    (when (string-index prefix char-whitespace?)
      (throw 'bad-raw-string-delimiter prefix))
    (unless (eqv? delim-begin (read-char port))
      (throw 'raw-string-delimiter-not-found prefix))
    (let search-delim ((c (char-please port)) (s '()))
      (if (eqv? delim-end c)
        (let search-delim-end ((ss (list delim-end)) (i 0))
          (if (= i (string-length prefix))
            (list->string (reverse! s))
            (let ((c (char-please port)))
              (if (eqv? (string-ref prefix i) c)
                (search-delim-end (cons c ss) (+ 1 i))
                (search-delim (char-please port) (append (cons c ss) s))))))
        (search-delim (char-please port) (cons c s))))))

(read-hash-extend #\R reader-extension-raw-string)
