
;; Reader extension for raw strings
;; by lloda@sarc.name 2017, 2019
;; This code is in the public domain. Feel free to use it as you please.

;; After R"delimiter( raw_characters )delimiter"
;; in http://en.cppreference.com/w/cpp/language/string_literal.
;; Having #R The quotes seemed unnecessary, so I consider them part of the
;; delimiter; you can ellide them. The characters ( ) can also be [ ] or " ".

(define-module (raw-strings)
  #:use-module ((ice-9 rdelim)))

(define (reader-extension-raw-string chr port)
; open-close choices
  (define delim-begin "([\"")
  (define (char-please port)
    (let ((c (read-char port)))
      (if (eof-object? c)
        (throw 'end-of-file-reading-raw-string)
        c)))
  (let* ((fix-open (read-delimited delim-begin port 'split))
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
                (search-delim (char-please port) (append (cons c ss) s))))))
        (search-delim (char-please port) (cons c s))))))

(read-hash-extend #\R reader-extension-raw-string)
