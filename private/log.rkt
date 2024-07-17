#lang racket/base

(require (for-syntax racket/base)
         racket/list
         racket/logging
         racket/mutable-treelist)

(provide html-printer-logger
         logging-enabled?
         log-err
         log-expr            ; For logging debug info at the xexpr-walking level
         log-printer         ; For logging debug info at the low-level printer level
         logging-to-stderr)

(module+ test)

(define-logger html-printer)
(define logging-enabled? (make-parameter #f))

(define (log-debug . items)
  (log-html-printer-debug (apply format items)))

(define (log-err . items)
  (log-html-printer-error (apply format items)))

(define (log-msg . items)
  (log-html-printer-info (apply format items)))

(define (logging-to-stderr proc)
  (with-logging-to-port (current-error-port) proc #:logger html-printer-logger 'debug 'html-printer))

(define-syntax (log-op stx)
  (syntax-case stx ()
    [(_ NESTLEVEL WHO OP MSG VAR ...)
     #'(log-debug "~a ~a~a • ~a "
                  ($indent WHO NESTLEVEL)
                  OP
                  (or (and MSG (format " ~a" MSG)) "")
                  ($vals (list '(VAR ...) `(,VAR ...))))]))

(define-syntax (log-expr stx)
  (syntax-case stx ()
    [(_ OP MSG VAR ...) #'(and (logging-enabled?) (log-op 0 "EXPR" 'OP 'MSG VAR ...))]))

(define-syntax (log-printer stx)
  (syntax-case stx ()
    [(_ LVL OP MSG VAR ...) #'(and (logging-enabled?) (log-op LVL "PRT" 'OP 'MSG VAR ...))]))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Formatting

(define ($indent who nestlevel)
  (cond
    [(= 0 nestlevel) who]
    [else
     (format "~a└─ ~a" (make-string (* 3 nestlevel) #\space) who)]))

(define ($var name val)
  (define output
    (cond ;[(number? val) (~r val #:min-width 3)]
      [(mutable-treelist? val) ($mtl val)]
      [else val]))
  (format "~a:~a" name output))

(define ($mtl m)
  (apply string-append
         (add-between (map (lambda (v) (format "~v" v)) (mutable-treelist->list m)) '(",")
                      #:before-first '("{")
                      #:after-last '("}")
                      #:splice? #t)))

(define ($vals lst)
  (apply string-append
         (add-between (map $var (car lst) (cadr lst)) " ")))
