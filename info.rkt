#lang info
(define collection "html-printer")
(define deps '("unicode-breaks"
               "base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/html-printer.scrbl" ())))
(define test-include-paths '("test.rkt"))
(define pkg-desc "A content aware pretty-printer for HTML5")
(define version "1.0")
(define pkg-authors '(joel))
(define license '(Apache-2.0 OR MIT))
