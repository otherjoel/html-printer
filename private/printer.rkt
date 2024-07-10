#lang racket/base

(require "strings.rkt"
         "log.rkt"
         racket/match
         (only-in racket/syntax format-symbol)
         unicode-breaks
         (rename-in racket/mutable-treelist
                    (mutable-treelist-first ~first)
                    (mutable-treelist-last ~last)
                    (mutable-treelist-add! ~add!)
                    (mutable-treelist-take! ~take!)
                    (mutable-treelist-drop! ~drop!)
                    (mutable-treelist-drop-right! ~drop-right!)
                    (mutable-treelist-empty? ~empty?)))

(provide make-wrapping-printer)

(module+ test)

(define (make-wrapping-printer [outp (current-output-port)]
                               #:wrap-at [wrap-col 100]
                               #:indent-spaces [indent 2])
  (let ([col 1]                   ; column of next char, counting actually-printed chars
        [indent-level 0]          ; indent level in columns
        [logical-line-start #t]   ; at "start"? (ignoring whitespace) counting actually-printed chars
        [accum-width 0]           ; length of strings in accumulator
        [accumulator (mutable-treelist)]) ; accumulator (list of strings not yet actually printed)

    (define (accum-empty?)
      (or (~empty? accumulator)
          (equal? accumulator (mutable-treelist breakpoint))))

    (define (end-is-whsp? mtl which-end)
      (and (not (~empty? mtl))
           (whitespace? ((if (eq? which-end 'right)
                             ~last
                             ~first)
                         mtl))))

    (define (lop-whsp-end! mtl which-end)
      (cond
        [(end-is-whsp? mtl which-end)
         (define-values (tl-get tl-drop)
           (case which-end
             [(left)  (values ~first ~drop!)]
             [(right) (values ~last ~drop-right!)]))
         (define lopped-width 0)
         (do () [(not (end-is-whsp? mtl which-end))]
           (set! lopped-width (+ lopped-width (string-grapheme-count (tl-get mtl))))
           (tl-drop mtl 1))
         lopped-width]
        [else #f]))
    
    (define (lop-accum-whsp-end! which-end)
      (match (lop-whsp-end! accumulator which-end)
        [(? number? lopped-len)
         (log-debug "accum width ~a - ~a (~a)" accum-width lopped-len which-end)
         (set! accum-width (- accum-width lopped-len))]
        [_ #f]))

    (define (flush!)
      (unless (accum-empty?)
        (log-debug "flushing accumulator: ~v" accumulator)
        (define lopped? (lop-accum-whsp-end! 'right))
        (define buffer (mutable-treelist))
        (define buf-width
          (for/fold ([held-whsp? #f]
                     [buffer-width 0]
                     #:result buffer-width)
                    ([v (in-mutable-treelist accumulator)])
            (cond
              [(breakpoint? v)
               (log-debug "flush (held ~a) - at breakpoint, buffer (col ~a + width ~a) ~v" held-whsp? col buffer-width buffer)
               (when (> (+ col buffer-width) wrap-col) (break+indent!))
               (cond
                 [(~empty? buffer) (values held-whsp? 0)]
                 [else
                  (log-debug "FLUSH held ~a, line start? ~a" held-whsp? logical-line-start)
                  (when (and held-whsp? (not logical-line-start)) (put! " "))
                  (for ([bv (in-mutable-treelist buffer)])
                    (unless (and logical-line-start (whitespace? bv))
                      (put! bv)))
                  (~take! buffer 0)
                  (values #f 0)])]
              [(whitespace? v)
               (log-debug "flush (held 1)")
               (values 1 (+ 1 buffer-width))]
              [else
               (when held-whsp? (~add! buffer " "))
               (~add! buffer v)
               (log-debug "flush (held was ~a) - buffer now ~v" held-whsp? buffer)
               (values #f (+ buffer-width (string-grapheme-count v)))])))
        
        (when (not (~empty? buffer))
          (when (> (+ col buf-width) wrap-col) (break+indent!))
          (for ([bv (in-mutable-treelist buffer)])
            (unless (and logical-line-start (whitespace? bv)) (put! bv))))

        (set! accum-width 0)
        (~take! accumulator 0)
        (when (and lopped? (not logical-line-start))
          (accumulate! " "))))
        
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

    (define (accumulate! v #:breakpoint-before? [breakpoint-before? #f])
      (define str (->string v))
      (define whsp? (whitespace? str))
      (define len (string-grapheme-count str))
      (unless (or (equal? "" str)
                  (and logical-line-start (accum-empty?) whsp?))
        (when breakpoint-before? (~add! accumulator breakpoint))
        (~add! accumulator str)
        (set! accum-width (+ accum-width len))
        (log-debug "Accumulator now (~a + ~a): ~v" col accum-width accumulator)))

    (define (accumulate/wrap! v)
      (define str (->string v))
      (when (> (+ col accum-width (string-grapheme-count str)) wrap-col)
        (flush!))
      (accumulate! str #:breakpoint-before? #t))

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
        (for/fold ([result (void)])
                  ([arg (in-list args)])
          (case arg
            [(get-line-length) wrap-col]
            [(break)
             (break!)]
            [(check/flush)
             (log-debug "check/flush, (+ ~a ~a) > ~a = ~a" col accum-width wrap-col
                        (> (+ col accum-width) wrap-col))
             (when (> (+ col accum-width) wrap-col) (flush!))]
            [(flush)
             (flush!)]
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
            [(pop-whitespace)
             (cond
               [(accum-empty?) #f]
               [else
                (log-debug "pop-whitespace - not empty…")
                (match (~last accumulator)
                  [(? whitespace? str)
                   (log-debug "popping whitespace ~v" str)
                   (~drop-right! accumulator 1)
                   (set! accum-width (- accum-width (string-grapheme-count str)))
                   str]
                  [_ #f])])]
            [else
             (when (and (list? arg) (not (null? arg)) (not (null? (cdr arg))))
               (define-values (sym str) (values (car arg) (cadr arg)))
               (case sym
                 [(put)
                  (cond
                    [(accum-empty?) (put! str)]
                    [else (accumulate! str)])]
                 [(put/wrap)
                  (accumulate/wrap! str)]))]))))
    (procedure-rename proc (format-symbol "wrapping-printer[cols:~a,sp:~a]" wrap-col indent))))
