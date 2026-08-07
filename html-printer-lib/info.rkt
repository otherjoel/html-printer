#lang info
(define collection "html-printer")
(define deps '(["base" #:version "8.13"]))
(define build-deps '("rackunit-lib"))
(define test-include-paths '("test.rkt"))
(define pkg-desc "A content aware pretty-printer for HTML5 (implementation)")
(define version "1.1")
(define pkg-authors '(joel))
(define license '(Apache-2.0 OR MIT))
