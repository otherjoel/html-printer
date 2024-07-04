#lang racket/base

(require "strings.rkt"
         "log.rkt"
         (only-in racket/syntax format-symbol)
         racket/mutable-treelist
         unicode-breaks)

(provide make-wrapping-printer)

(define (make-wrapping-printer [outp (current-output-port)]
                               #:wrap-at [wrap-col 100]
                               #:indent-spaces [indent 2])
  
  (let ([col 1]                   ; column of next char, counting actually-printed chars
        [indent-level 0]          ; indent level in columns
        [logical-line-start #t]   ; at "start"? (ignoring whitespace) counting actually-printed chars
        [accum-width 0]           ; length of strings in accumulator
        [accumulator (mutable-treelist)]) ; accumulator (list of strings not yet actually printed)

    (define (accum-empty?)
      (mutable-treelist-empty? accumulator))

    (define (accum-end-is-whsp? which-end)
      (and (not (accum-empty?))
           (whitespace? ((if (eq? which-end 'right)
                             mutable-treelist-last
                             mutable-treelist-first)
                         accumulator))))

    (define (lop-accum-whsp-end! which-end)
      (cond
        [(and (not (accum-empty?))
              (accum-end-is-whsp? which-end))
         (define-values (tl-get tl-drop)
           (case which-end
             [(left)  (values mutable-treelist-first mutable-treelist-drop!)]
             [(right) (values mutable-treelist-last mutable-treelist-drop-right!)]))
         (do () [(not (accum-end-is-whsp? which-end))]
           (let ([last-len (string-grapheme-count (tl-get accumulator))])
             (log-debug "accum width ~a - ~a (~a)" accum-width last-len which-end)
             (set! accum-width (- accum-width last-len))
             (tl-drop accumulator 1)))
         #t]
        [else #f]))

    (define (flush!)
      (unless (accum-empty?)
        (log-debug "flushing accumulator: ~v" accumulator)
        (define lopped? (lop-accum-whsp-end! 'right))
        (for ([v (in-mutable-treelist accumulator)])
          (display v outp)
          (when (and logical-line-start (not (whitespace? v)))
            (set! logical-line-start #f)))
        (set! col (+ col accum-width))
        (set! accum-width 0)
        (mutable-treelist-take! accumulator 0)
        (when (and lopped? (not logical-line-start))
          (accumulate! " "))))
    
    (define (flush-accumulator!)
      (unless (accum-empty?)
        (when (and (not logical-line-start) (> (+ col accum-width) wrap-col)) (break+indent!))
        (flush!)))
        
    (define (break!)
      (log-debug "break!")
      (flush!)
      (display (sys-newline) outp)
      (set! logical-line-start #t)
      (set! col 1))
    
    (define (break+indent!)
      (log-debug "break+indent!")
      (display (sys-newline) outp)
      (display (make-string indent-level #\space) outp)
      (set! logical-line-start #t)
      (set! col (+ 1 indent-level))
      (lop-accum-whsp-end! 'left))

    (define (accumulate! v)
      (define str (->string v))
      (define whsp? (whitespace? str))
      (define len (string-grapheme-count str))
      (unless (or (equal? "" str)
                  (and logical-line-start (accum-empty?) whsp?))
        (mutable-treelist-add! accumulator str)
        (set! accum-width (+ accum-width len))
        (log-debug "Accumulator now (~a + ~a): ~v" col accum-width accumulator)))

    (define (accumulate/wrap! v)
      (cond
        [(> (+ col accum-width (string-grapheme-count v)) wrap-col)
         (flush-accumulator!)
         (accumulate! v)]
        [else (accumulate! v)]))

    (define (put! v)
      (define str (->string v))
      (unless (whitespace? str) (set! logical-line-start #f))
      (set! col
            (for/fold ([c col])
                      ([w (in-words str)])
              (display w outp)
              (cond [(linebreak? w)
                     (set! logical-line-start #t)
                     1]
                    [else (+ c (string-grapheme-count w))]))))
    
    (define proc
      (lambda args
        (unless (null? args)
          (define arg (car args))
          (case arg
            [(get-line-length) wrap-col]
            [(break)
             (break!)]
            [(check/flush)
             (log-debug "check/flush, (+ ~a ~a) > ~a = ~a" col accum-width wrap-col
                        (> (+ col accum-width) wrap-col))
             (when (> (+ col accum-width) wrap-col) (flush-accumulator!))]
            [(flush)
             (flush-accumulator!)]
            [(indent)
             (display (make-string indent-level #\space) outp)
             (set! col (+ col indent-level))]
            [(indent-if-col1)
             (when (= col 1)
               (display (make-string indent-level #\space) outp)
               (set! col (+ col indent-level)))]
            [(--indent)
             (log-debug "(--indent)")
             (set! indent-level (max 0 (- indent-level indent)))
             (display (make-string indent-level #\space) outp)
             (set! col (+ col indent-level))]
            [(break/indent)
             (break+indent!)]
            [(break/++indent)
             (log-debug "(break/++indent)")
             (set! indent-level (+ indent-level indent))
             (break+indent!)]
            [(break/--indent)
             (log-debug "(break/--indent)")
             (set! indent-level (max 0 (- indent-level indent)))
             (break+indent!)]
            [else
             (when (and (list? arg) (not (null? arg)) (not (null? (cdr arg))))
               (define-values (sym str) (values (car arg) (cadr arg)))
               (case sym
                 [(put)
                  (cond
                    [(accum-empty?) (put! str)]
                    [else (accumulate! str)])]
                 [(put/wrap)
                  (accumulate/wrap! str)]))])
          (unless (null? (cdr args)) (apply proc (cdr args))))))
    (procedure-rename proc (format-symbol "wrapping-printer[cols:~a,sp:~a]" wrap-col indent))))
