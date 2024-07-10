#lang racket/base

(require "../main.rkt"
         "strings.rkt"
         "tidy.rkt"
         "log.rkt"
         racket/string
         rackunit)

(provide (all-defined-out))

(module+ test)

;; Convert output to lists of strings for use in tests
(define (->strs v) (string-split (xexpr->html5 v #:wrap-at 20) (sys-newline)))

(define-check (check-fmt width msg xpr strs)
  (define my-result (xexpr->html5 xpr #:wrap width))
  (define standard (string-join strs (sys-newline)))
  (unless (equal? my-result standard)
    (with-check-info (['message (string-info msg)]
                      ['|writer result| (string-info (w/rule width my-result))]
                      ['expected (string-info (w/rule width standard))])
      (fail-check))))

;; Check the writer's result against the output of HTML Tidy.
;; If a sufficiently new version of HTML Tidy is not installed, check passes.
(define-check (check-matches-tidy? width x)
  (when (tidy-version-sufficient?)
    (define my-result (xexpr->html5 (xpr x) #:wrap width))
    (define tidy-result (string-append (tidy x #:wrap width) "\n"))
    (unless (equal? my-result tidy-result)
      (with-check-info (['message (string-info "writer result does not match expected tidy output")]
                        ['|writer result| (string-info (w/rule width my-result))]
                        ['|tidy output| (string-info (w/rule width tidy-result))])
        (fail-check)))))

;; 25 → "----|----1----|----2----|"
(define (rule n)
  (list->string
   (for/list ([i (in-range n)])
     (define col (+ 1 i))
     (cond
       [(eqv? 0 (modulo col 10))
        (integer->char (+ 48 (/ col 10)))] ; 48 = #\0
       [(eqv? 0 (modulo col 5))
        #\|]
       [else #\-]))))        

(define (w/rule width str)
  (string-append (sys-newline) (rule (+ width 15)) (sys-newline)
                 (string-replace
                  (string-replace str " " "·")
                  (sys-newline) (string-append "¶" (sys-newline)))))

(define (proof v #:wrap [wrap 20])
  (display (w/rule wrap (xexpr->html5 v #:wrap wrap))))

;; Print v as HTML and show all the debug-level logging
(define (debug v #:wrap [wrap 20])
  (display (w/rule wrap (logging-to-stderr (lambda () (xexpr->html5 v #:wrap wrap))))))