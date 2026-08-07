#lang racket/base

(require "../main.rkt"
         "../private/strings.rkt"
         "../private/tidy.rkt"
         "../private/debug-util.rkt"
         racket/string
         rackunit)

(provide (all-defined-out)
         (all-from-out "../private/debug-util.rkt")
         check-exn check-true check-false)

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
