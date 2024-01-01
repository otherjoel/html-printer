#lang racket/base

(require "strings.rkt"
         "log.rkt"
         (only-in racket/syntax format-symbol)
         unicode-breaks)

(provide make-wrapping-printer)

(define (make-wrapping-printer [outp (current-output-port)]
                               #:wrap-at [wrap-col 100]
                               #:indent-spaces [indent 2])
  (let ([col 1]                           
        [indent-level 0]
        [logical-line-start #t])
    (define (break+indent!)
      (display (sys-newline) outp)
      (display (make-string indent-level #\space) outp)
      (set! logical-line-start #t)
      (set! col (+ 1 indent-level)))
    (define proc
      (lambda args
        (unless (null? args)
          (define arg (car args))
          (case arg
            [(break)
             (display (sys-newline) outp)
             (set! logical-line-start #t)
             (set! col 1)]
            [(indent)
             (display (make-string indent-level #\space) outp)
             (set! col (+ col indent-level))]
            [(indent-if-col1)
             (when (= col 1)
               (display (make-string indent-level #\space) outp)
               (set! col (+ col indent-level)))]
            [(--indent)
             (set! indent-level (max 0 (- indent-level indent)))
             (display (make-string indent-level #\space) outp)
             (set! col (+ col indent-level))]
            [(break/indent)
             (break+indent!)]
            [(break/++indent)
             (set! indent-level (+ indent-level indent))
             (break+indent!)]
            [(break/--indent)
             (set! indent-level (max 0 (- indent-level indent)))
             (break+indent!)]
            [else
             (when (and (list? arg) (not (null? arg)) (not (null? (cdr arg))))
               (define-values (sym str) (values (car arg) (cadr arg)))
               (case (car arg)
                 [(put)
                  (unless (whitespace? str) (set! logical-line-start #f))
                  (set! col
                        (for/fold ([c col])
                                  ([w (in-words str)])
                          (display w outp)
                          (cond [(linebreak? w)
                                 (set! logical-line-start #t)
                                 1]
                                [else (+ c (string-grapheme-count w))])))]
                 [(put/wrap)
                  (define len (string-grapheme-count str))
                  (define whsp? (whitespace? str))
                  (cond [(> (+ col len) wrap-col) (break+indent!)])
                  (unless (and logical-line-start whsp?)
                    (display str outp)
                    (set! col (+ col len)))
                  (unless whsp? (set! logical-line-start #f))]))])
          (unless (null? (cdr args)) (apply proc (cdr args))))))
    (procedure-rename proc (format-symbol "wrapping-printer[cols:~a,sp:~a]" wrap-col indent))))
