#lang racket/base

(require "strings.rkt"
         "log.rkt"
         racket/class
         unicode-breaks)

(provide wrapping-printer%)

(define (bump n) (make-string n #\space))

;; Does its own column counting, because port-next-location counts bytes not graphemes
(define wrapping-printer%
  (class object%
    (super-new)
    (init [output-port (current-output-port)] [wrap 100])
    (define outp output-port)
    (define wrap-col wrap)
    (define col 1)
    (define indent 0)
    (define indent-spaces 2)
    (define logical-line-start #t)

    (define/public (get-col) col)

    ;; print str and track col but do not auto-wrap
    (define/public (put! str)
      (unless (whitespace? str) (set! logical-line-start #f))
      (set! col
            (for/fold ([c col])
                      ([w (in-words str)])
              (display w outp)
              (if (linebreak? w) 1 (+ c (string-grapheme-count w))))))

    ;; print str, tracking col and wrapping when needed
    (define/public (put/wrap! str)
      (define len (string-grapheme-count str))
      (define whsp? (whitespace? str))
      (cond [(> (+ col len) wrap-col) (break/indent!)])
      (when (and logical-line-start whsp?) (log-debug "DISCARDING ~s" str))
      (unless (and logical-line-start whsp?) ; discard whitespace at beginning of line
        (display str outp)
        (set! col (+ col len)))
      (unless whsp? (set! logical-line-start #f)))

    ;; line break only
    (define/public (break!)
      (display (sys-newline) outp)
      (set! logical-line-start #t)
      (set! col 1))

    ;; line break at current indent level
    (define/public (break/indent!)
      (display (string-append (sys-newline) (bump indent)) outp)
      (set! logical-line-start #t)
      (set! col (+ 1 indent)))

    (define/public (indent!)
      (display (bump indent) outp)
      (set! col (+ col indent)))

    ;; line break and increase indent level
    (define/public (break+bump!)
      (set! indent (+ indent indent-spaces))
      (display (string-append (sys-newline) (bump indent)) outp)
      (set! logical-line-start #t)
      (set! col (+ 1 indent)))

    ;; line break and decrease indent level
    (define/public (break+unbump!)
      (set! indent (max 0 (- indent indent-spaces)))
      (display (string-append (sys-newline) (bump indent)) outp)
      (set! logical-line-start #t)
      (set! col (+ 1 indent)))

    ;; indent at reduced level
    (define/public (indent/unbump!)
      (set! indent (max 0 (- indent indent-spaces)))
      (display (bump indent) outp)
      (set! logical-line-start #t)
      (set! col (+ col indent)))
    
    ))
