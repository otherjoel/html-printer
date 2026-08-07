#lang racket/base

(require "../main.rkt"
         "strings.rkt"
         "tidy.rkt"
         "log.rkt"
         racket/string)

(provide (all-defined-out))

(module+ test)

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

(define (proof v #:wrap [wrap 20] #:add-breaks? [br? #t])
  (display (w/rule wrap (xexpr->html5 v #:wrap wrap #:add-breaks? br?))))

;; Print v as HTML and show all the debug-level logging
(define (debug v #:wrap [wrap 20] #:add-breaks? [br? #t])
  (display
   (w/rule wrap
           (logging-to-stderr
            (lambda () (parameterize ([logging-enabled? #t])
                         (xexpr->html5 v #:wrap wrap #:add-breaks? br?)))))))

(define (compare/tidy! width x)
  (cond
    [(tidy-version-sufficient?)
     (printf "Tidy ~a found\n" (get-tidy-version))
     (define my-result (xexpr->html5 (xpr x) #:wrap width))
     (define tidy-result (string-append (tidy x #:wrap width) "\n"))
     (cond
       [(equal? my-result tidy-result)
        (printf "Results match:\n")
        (display (w/rule width my-result))]
       [else
        (printf "Results do not match\nxexpr->html5 result:\n")
        (display (w/rule width my-result))
        (printf "Tidy result:\n")
        (display (w/rule width tidy-result))])]
    [else
     (raise-user-error 'compare/tidy! "Tidy >= ~a not found in HTML_TIDY_PATH or PATH\n" minimum-tidy-version)]))
