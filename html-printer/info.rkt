#lang info
(define collection "html-printer")
(define implies    '("html-printer-lib"))
(define build-deps '("racket-doc"
                     "scribble-lib"
                     "txexpr"))
(define scribblings '(("scribblings/html-printer.scrbl" ())))
(define pkg-desc "Business Central REST API client library")
(define version "0.1")
(define pkg-authors '(joel))
(define license '(Apache-2.0 OR MIT))
(define deps '("base"
               "html-printer-lib"))
