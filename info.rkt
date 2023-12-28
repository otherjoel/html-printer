#lang info
(define collection "html-writer")
(define deps '("unicode-breaks"
               "base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/html-writer.scrbl" ())))
(define pkg-desc "A content aware pretty-printer for HTML5")
(define version "0.1")
(define pkg-authors '(joel))
(define license '(Apache-2.0 OR MIT))
