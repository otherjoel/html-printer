#lang racket/base

(require "../main.rkt"
         "strings.rkt"
         "tidy.rkt"
         "log.rkt"
         racket/string
         rackunit)

(provide (all-defined-out) check-exn check-true check-false)

(module+ test)

;; Convert output to lists of strings for use in tests
(define (->strs v [wrap 20]) (string-split (xexpr->html5 v #:wrap wrap) (sys-newline)))

(define-check (check-fmt width msg xpr strs)
  (define my-result (xexpr->html5 xpr #:wrap width))
  (define standard (string-join strs (sys-newline)))
  (unless (equal? my-result standard)
    (with-check-info (['message (string-info msg)]
                      ['|writer result| (string-info (w/rule width my-result))]
                      ['expected (string-info (w/rule width standard))])
      (fail-check))))

(define-check (check-fmt-addbrs width msg xpr strs)
  (define my-result (xexpr->html5 xpr #:wrap width #:add-breaks? #t))
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
