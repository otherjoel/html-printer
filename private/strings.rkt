#lang racket/base

(require racket/match)

(provide whitespace?
         linebreak?
         newline-convention
         sys-newline
         escape
         string-element-table
         attribute-table)

(define newline-convention (make-parameter (system-type)))

(define (sys-newline)
  (if (eq? (newline-convention) 'windows) "\r\n" "\n"))

;; Test if a string consists entirely of whitespace.
;; NB: A non-breaking space is not counted as whitespace.
(define (whitespace? s)
  (match s
    ["" #t]
    [(pregexp #px"^[\\s]+$") #t]
    [_ #f]))

;; Test if a string consists only of \r or \n.
(define (linebreak? s)
  (regexp-match? #rx"^[\r\n]+$" s))

(module+ test
  (require rackunit)
  (check-true (whitespace? "")   "empty string is whitespace")
  (check-true (whitespace? "  ") "spaces are whitespace")
  (check-true (whitespace? "\t") "tab is whitespace")
  (check-true (whitespace? "\r\n") "newlines and carriage returns are whitespace")
  (check-false (whitespace? "\u00A0") "non-breaking space is not whitespace")
  (check-false (whitespace? " x ") "a single non-whitespace character means whole string is not whitespace")
  (check-false (whitespace? (string->symbol " ")) "non-strings are never whitespace")

  (check-true (linebreak? "\r\n") "carriage returns and newlines are line breaks")
  (check-true (linebreak? "\r\r\n\r\n\n") "CR and LF in any amount and order are line breaks")
  (check-false (linebreak? " \r\n") "any non-CRLF character disqualifies string as a line break")
  )

;;
;; XML Escapes
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Copied from https://github.com/racket/racket/blob/60f7edb0e59d50c65e59bdb21e4955cde892acd9/racket/collects/xml/private/writer.rkt#L191C1-L214C35
;; Avoiding dependency on xml collection for now.

(define string-element-table #px"[<>&]")
(define attribute-table #rx"[<>&\"]")

(define (replace-escaped s)
  (define c (string-ref s 0))
  (case c
    [(#\<) "&lt;"]
    [(#\>) "&gt;"]
    [(#\&) "&amp;"]
    [(#\") "&quot;"]
    [else c]))

(define (escape str table)
  (cond [(regexp-match table str)
         (regexp-replace* table str replace-escaped)]
        [else str]))