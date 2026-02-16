#lang racket/base

(require "strings.rkt"
         "log.rkt"
         racket/match
         (only-in racket/syntax format-symbol)
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
    
    (define (lop-accum-whsp-end! which-end [lev 1])
      (match (lop-whsp-end! accumulator which-end)
        [(? number? lopped-len)
         (log-printer lev lop-accum-end _ accum-width lopped-len which-end)
         (set! accum-width (- accum-width lopped-len))]
        [_ #f]))

    (define (flush! [lev 1])
      (unless (accum-empty?)
        (log-printer lev flush start… col accum-width logical-line-start indent-level accumulator)
        (define lopped? (lop-accum-whsp-end! 'right (+ lev 1)))
        (define buffer (mutable-treelist))
        (define buf-width
          (for/fold ([held-whsp? #f]
                     [buffer-width 0]
                     #:result buffer-width)
                    ([v (in-mutable-treelist accumulator)])
            (cond
              [(breakpoint? v)
               (log-printer (+ lev 1) flush at_bp col buffer-width held-whsp? buffer)
               (when (> (+ col buffer-width) wrap-col) (break+indent! (+ lev 1)))
               (cond
                 [(~empty? buffer) (values held-whsp? 0)]
                 [else
                  (log-printer (+ lev 2) flush printbuf… held-whsp? logical-line-start indent-level)
                  (for ([bv (in-mutable-treelist buffer)])
                    (unless (and logical-line-start (whitespace? bv))
                      (put! bv (+ lev 3))))
                  (when (and held-whsp? (not logical-line-start)) (put! " "))
                  (~take! buffer 0)
                  (values #f 0)])]
              [(whitespace? v)
               (log-printer (+ lev 1) flush whitespace v)
               (values 1 (+ 1 buffer-width))]
              [else
               (when held-whsp? (~add! buffer " "))
               (~add! buffer v)
               (log-printer (+ lev 1) flush non-whsp held-whsp? buffer)
               (values #f (+ buffer-width (string-grapheme-count v)))])))
        
        (log-printer lev flush done-breaking col accum-width logical-line-start accumulator)
        (when (not (~empty? buffer))
          (when (> (+ col buf-width) wrap-col) (break+indent! (+ lev 1)))
          (for ([bv (in-mutable-treelist buffer)])
            (unless (and logical-line-start (whitespace? bv)) (put! bv (+ lev 1)))))

        (set! accum-width 0)
        (~take! accumulator 0)
        (when (and lopped? (not logical-line-start))
          (accumulate! " ")
          (log-printer lev flush done col accum-width logical-line-start accumulator)
          )))
        
    (define (break! [lev 1])
      (log-printer lev break! start… col accum-width logical-line-start indent-level accumulator)
      (flush! (+ lev 1))
      (display (sys-newline) outp)
      (set! logical-line-start #t)
      (set! col 1)
      (log-printer lev break! …end col accum-width logical-line-start indent-level accumulator))
    
    (define (break+indent! [lev 1])
      (log-printer lev break+indent! start… col accum-width logical-line-start indent-level accumulator)
      (display (sys-newline) outp)
      (display (make-string indent-level #\space) outp)
      (set! logical-line-start #t)
      (set! col (+ 1 indent-level))
      (lop-accum-whsp-end! 'left (+ lev 1))
      (log-printer lev break+indent! …end col accum-width logical-line-start indent-level accumulator))

    (define (accumulate! v [lev 1] #:breakpoint-before? [breakpoint-before? #f])
      (log-printer lev accum! start… col accum-width logical-line-start indent-level breakpoint-before? accumulator)
      (define str (->string v))
      (define whsp? (whitespace? str))
      (define len (string-grapheme-count str))
      (unless (or (equal? "" str)
                  (and logical-line-start (accum-empty?) whsp?))
        (when breakpoint-before? (~add! accumulator breakpoint))
        (~add! accumulator str)
        (set! accum-width (+ accum-width len)))
      (log-printer lev accum! …end col accum-width logical-line-start indent-level accumulator))

    (define (accumulate/wrap! v [lev 1])
      (log-printer lev accum/wrap! start… col accum-width logical-line-start indent-level accumulator)
      (define str (->string v))
      (when (> (+ col accum-width (string-grapheme-count str)) wrap-col)
        (flush! (+ 1 lev)))
      (accumulate! str (+ lev 1) #:breakpoint-before? #t)
      (log-printer lev accum/wrap! …end col accum-width logical-line-start indent-level accumulator))

    (define (put! v [lev 1])
      (log-printer lev put! start… v col accum-width logical-line-start indent-level)
      (define str (->string v))
      (unless (whitespace? str) (set! logical-line-start #f))
      (set! col
            (for/fold ([c col])
                      ([w (in-list (words str))])
              (display w outp)
              (cond [(linebreak? w)
                     (set! logical-line-start #t)
                     1]
                    [else (+ c (string-grapheme-count w))])))
      (log-printer lev put! …end col accum-width logical-line-start indent-level))
    
    (define proc
      (lambda args
        (for/fold ([result (void)])
                  ([arg (in-list args)])
          (case arg
            [(get-line-length) wrap-col]
            [(break)
             (break!)]
            [(check/flush)
             (log-printer 1 check/flush col accum-width wrap-col indent-level)
             (when (> (+ col accum-width) wrap-col) (flush! 2))]
            [(flush)
             (flush!)]
            [(indent)
             (log-printer 1 indent start col indent-level)
             (display (make-string indent-level #\space) outp)
             (set! col (+ col indent-level))
             (log-printer 1 indent end col indent-level)]
            [(indent-if-col1)
             (log-printer 1 indent-if-col1 start col indent-level)
             (when (= col 1)
               (display (make-string indent-level #\space) outp)
               (set! col (+ col indent-level)))
             (log-printer 1 indent-if-col1 end col indent-level)]
            [(--indent)
             (log-printer 1 --indent start col indent-level)
             (set! indent-level (max 0 (- indent-level indent)))
             (display (make-string indent-level #\space) outp)
             (set! col (+ col indent-level))
             (log-printer 1 --indent end col indent-level)]
            [(break/indent)
             (break+indent!)]
            [(break/++indent)
             (log-printer 1 break/++indent start col indent-level)
             (set! indent-level (+ indent-level indent))
             (break+indent! 2)
             (log-printer 1 break/++indent end col indent-level)]
            [(break/--indent)
             (log-printer 1 break/--indent _ col)
             (set! indent-level (max 0 (- indent-level indent)))
             (break+indent! 2)]
            [(pop-whitespace)
             (log-printer 1 pop-whitespace _ accumulator)
             (cond
               [(accum-empty?) #f]
               [else
                (match (~last accumulator)
                  [(? whitespace? popped)
                   (~drop-right! accumulator 1)
                   (set! accum-width (- accum-width (string-grapheme-count popped)))
                   (log-printer 1 pop-whitespace _ popped accumulator)
                   popped]
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
