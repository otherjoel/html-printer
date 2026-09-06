#lang racket/base

(require "strings.rkt"
         "log.rkt"
         racket/match
         (only-in racket/syntax format-symbol))

(provide make-wrapping-printer
         (struct-out text)
         (struct-out raw))

(module+ test)

;; The printer consumes a flat stream of tokens and lays them out greedily, one line at a time:
;;
;;   (text str)  An unbreakable run of characters. Adjacent text tokens are glued into one
;;               "cluster" which is always printed on a single line.
;;   'space      A break opportunity that prints as a single space if the line continues.
;;               Consecutive spaces collapse; a space at the start or end of a line is dropped.
;;   'softbreak  A zero-width break opportunity: prints nothing unless the line breaks there.
;;   'newline    A hard line break. Does nothing if nothing has been printed on the current line.
;;   'blank      A hard line break that always produces an empty line.
;;   'indent+    Raise/lower the indent level. Takes effect the next time a line is started.
;;   'indent-
;;   (raw str indent?)
;;               Verbatim output (contents and closing tags of <pre>, <script>, <style>). Never
;;               wrapped; the printer only keeps track of the column it ends in. If it arrives at
;;               the start of a line, the indent is written first when indent? is true (closing
;;               tags of <script> and <style> after content ending in a newline) and omitted when
;;               it is false (content, and the closing tag of <pre>, where indent would be content).
;;   'flush      Print whatever is still pending (used at the end of the document).
;;
;; A list of tokens is accepted anywhere a token is.

(struct text (str) #:transparent)
(struct raw (str indent?) #:transparent)

(define (make-wrapping-printer [outp (current-output-port)]
                               #:wrap-at [wrap-col 100]
                               #:indent-spaces [indent 2])
  (define col 1)              ; 1-based column at which the next character will land
  (define indent-level 0)     ; current indent, in columns
  (define line-start? #t)     ; nothing written on the current line yet (indent is written lazily)
  (define pending 'none)      ; separator owed before the next cluster: 'none, 'softbreak or 'space
  (define cluster '())        ; text glued together since the last break opportunity (reversed)
  (define cluster-width 0)    ; width of cluster, in graphemes

  (define (write! s) (display s outp))

  (define (newline!)
    (write! (sys-newline))
    (set! col 1)
    (set! line-start? #t)
    (set! pending 'none))

  ;; Write the indent for a fresh line. Only called when something is about to be printed.
  (define (start-line!)
    (write! (make-string indent-level #\space))
    (set! col (+ 1 indent-level))
    (set! line-start? #f))

  ;; Print the pending cluster, breaking the line first if it would not fit. The cluster (with its
  ;; separating space, if any) will occupy columns col … col+needed-1, so it fits exactly when
  ;; col+needed-1 <= wrap-col. Nothing is ever broken at the start of a line: a cluster wider than
  ;; the whole line simply overflows.
  (define (commit!)
    (unless (null? cluster)
      (define sep? (and (eq? pending 'space) (not line-start?)))
      (define needed (+ cluster-width (if sep? 1 0)))
      (define fits? (or line-start? (<= (+ col needed -1) wrap-col)))
      (log-printer 1 commit _ col cluster-width sep? fits? line-start? indent-level cluster)
      (unless fits? (newline!))
      (cond
        [line-start? (start-line!)]
        [sep? (write! " ") (set! col (+ col 1))])
      (for ([s (in-list (reverse cluster))]) (write! s))
      (set! col (+ col cluster-width))
      (set! cluster '())
      (set! cluster-width 0)
      (set! pending 'none)))

  (define (text! s)
    (unless (equal? s "")
      (set! cluster (cons s cluster))
      (set! cluster-width (+ cluster-width (string-grapheme-count s)))))

  (define (raw! s indent?)
    (commit!)
    (set! pending 'none)
    (unless (equal? s "")
      (when line-start?
        (if indent? (start-line!) (set! line-start? #f)))
      (write! s)
      (define tail (car (regexp-match #rx"[^\r\n]*$" s)))
      (cond
        [(= (string-length tail) (string-length s)) ; no line breaks inside s
         (set! col (+ col (string-grapheme-count s)))]
        [(equal? tail "")
         (set! col 1)
         (set! line-start? #t)]
        [else
         (set! col (+ 1 (string-grapheme-count tail)))])))

  (define (handle! tok)
    (log-printer 1 token _ tok col line-start? pending cluster-width indent-level)
    (match tok
      [(text s) (text! s)]
      [(raw s indent?) (raw! s indent?)]
      ['space (commit!) (set! pending 'space)]
      ['softbreak (commit!) (when (eq? pending 'none) (set! pending 'softbreak))]
      ['newline (commit!) (if line-start? (set! pending 'none) (newline!))]
      ['blank (commit!) (unless line-start? (newline!)) (newline!)]
      ['indent+ (commit!) (set! indent-level (+ indent-level indent))]
      ['indent- (commit!) (set! indent-level (max 0 (- indent-level indent)))]
      ['flush (commit!)]
      [(? list? toks) (for-each handle! toks)]
      [_ (raise-argument-error 'wrapping-printer "printer token" tok)]))

  (procedure-rename (lambda toks (for-each handle! toks))
                    (format-symbol "wrapping-printer[cols:~a,sp:~a]" wrap-col indent)))
