; -*- mode: scheme; coding: utf-8 -*-
;; Reader extension for raw strings
;; by lloda@sarc.name 2017, 2019, 2022
;; This code is in the public domain.

;; Based on R"delimiter(raw_characters)delimiter"
;; in http://en.cppreference.com/w/cpp/language/string_literal.
;; Having #R, the quotes are unnecessary, so I consider them part of the delimiter;
;; you can ellide them. The open-close characters can be (), [], or "".

(define-module (raw-strings)
  #:use-module ((ice-9 rdelim)))

(define (reader-extension-raw-string chr port)
; open-close choices
  (define oc "([\"")
  (define (char-please port)
    (let ((c (read-char port)))
      (if (eof-object? c)
        (throw 'end-of-file-reading-raw-string)
        c)))
  (let* ((fix-open (read-delimited oc port 'split))
         (fix (car fix-open))
         (open (cdr fix-open))
; match open-close characters
         (close (case open
                  ((#\() #\)) ((#\[) #\]) ((#\") #\")
                  (else (throw 'raw-string-delimiter-not-found fix)))))
    (when (string-index fix char-whitespace?)
      (throw 'raw-string-delimiter-has-whitespace fix))
    (let search-delim ((c (char-please port)) (s '()))
      (if (eqv? close c)
        (let search-close ((ss (list close)) (i 0))
          (if (= i (string-length fix))
            (list->string (reverse! s))
            (let ((c (char-please port)))
              (if (eqv? (string-ref fix i) c)
                (search-close (cons c ss) (+ 1 i))
                (search-delim c (append ss s))))))
        (search-delim (char-please port) (cons c s))))))

(read-hash-extend #\R reader-extension-raw-string)
