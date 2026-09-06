#lang racket/base

;; Property-based check of xexpr->html5. Random documents are printed at random widths, and every
;; result must satisfy three properties the documentation promises:
;;
;;  1. It renders like the input. After dropping comments and inline tags (whitespace collapses
;;     across both), reducing the remaining tags to bare names, and collapsing whitespace, the
;;     output must equal xexpr->string of the same input.
;;  2. No line ends in whitespace, and without #:add-breaks? no line is empty.
;;  3. A line longer than the wrap width contains no break opportunity: no whitespace other than
;;     the space in a glued `<tag attr=` opener or spaces inside a quoted attribute value.
;;
;; <pre>, <script> and <style> are left out of the generator because property 1 cannot account
;; for verbatim content; test.rkt covers them directly.
;;
;; A given seed always produces the same documents, so the check is deterministic. Two environment
;; variables override the arguments for a harder local run:
;;   HTML_PRINTER_FUZZ_COUNT   number of documents to check
;;   HTML_PRINTER_FUZZ_SEED    an integer, or "random" to draw a fresh seed (reported on failure)

(require "../main.rkt"
         "../private/html-tags.rkt"
         "../private/debug-util.rkt" ; for w/rule
         racket/list
         racket/string
         rackunit
         xml)

(provide check-fuzz
         current-fuzz-printer)

(module+ test)

;; parameter allows failure reporting to be exercised against a deliberately broken printer.
(define current-fuzz-printer (make-parameter xexpr->html5))

;;================================================
;; Random documents

(define inline-tags '(em strong a span b i code))
(define flow-tags (filter (lambda (t) (not (self-closing? t))) default-flow-tags))
(define block-tags (filter (lambda (t) (not (self-closing? t))) default-block-tags))

(define vocab
  '("lorem" "ipsum" "dolor" "sit" "amet" "consectetur" "adipiscing" "elit" "x"
    "averyveryverylongwordthatdoesnotfitanywhere" "a&b" "<tag>" "q\"uote" "🧝‍♂️🧙🏽‍♂️"))
(define gaps '(" " "  " "\n" " \n "))
(define edges '("" "" " " "\n"))
(define whitespace-strings '(" " "\n" "  " " \n "))

(define (pick lst) (list-ref lst (random (length lst))))

(define (random-text)
  (string-append (pick edges)
                 (string-join (for/list ([i (in-range (random 1 5))]) (pick vocab)) (pick gaps))
                 (pick edges)))

(define (random-attrs)
  (case (random 4)
    [(0) '()]
    [(1) '((class "x"))]
    [(2) '((href "https://example.com/some/path") (class "a b"))]
    [else '((disabled "") (id "z"))]))

(define (random-element depth)
  (define r (random 12))
  (cond
    [(< r 1) (pick whitespace-strings)]
    [(or (<= depth 0) (< r 4)) (random-text)]
    [(< r 5) (pick '(nbsp mdash 65))]
    [(< r 6) (comment "a comment\nhere")]
    [(< r 7) '(br)]
    [(< r 9) `(,(pick inline-tags) ,(random-attrs) ,@(random-children (- depth 1)))]
    [(< r 10) `(,(pick block-tags) ,(random-attrs) ,@(random-children (- depth 1)))]
    [else `(,(pick flow-tags) ,(random-attrs) ,@(random-children (- depth 1)))]))

(define (random-children depth)
  (for/list ([i (in-range (random 0 5))]) (random-element depth)))

(define (random-document)
  `(body () ,@(random-children 4)))

;;================================================
;; Properties

(define inline-tag-rx
  (pregexp (string-append "</?(?:" (string-join (map symbol->string inline-tags) "|")
                         ")(?:\\s[^>]*)?>")))

(define (canonical html)
  (let* ([s (regexp-replace* #rx"<!--.*?-->" html "")]                       ; comments do not render
         [s (regexp-replace* inline-tag-rx s "")]                            ; whitespace collapses across inline tags
         [s (regexp-replace* #px"<(/?)([a-zA-Z0-9-]+)[^>]*>" s "<\\1\\2>")]  ; bare tag names; also <br /> → <br>
         [s (regexp-replace* #px"\\s+" s " ")]                               ; whitespace runs collapse
         [s (regexp-replace* #px"\\s*(<[^>]+>)\\s*" s "\\1")])               ; whitespace next to a block tag is insignificant
    s))

;; Does a line contain whitespace that could have been a break opportunity? Everything counts
;; except the space in a glued `<tag attr` opener and spaces inside quoted attribute values.
(define (breakable-whitespace? line)
  (define masked
    (regexp-replace* #px"\"[^\"]*\""
                     (regexp-replace* #px"<([a-zA-Z0-9-]+) ([a-zA-Z0-9-]+)" line "<\\1_\\2")
                     "\"\""))
  (not (regexp-match? #px"^\\s*\\S+$" masked)))

;; Lines of the output, not counting the final line break
(define (output-lines out)
  (string-split (string-trim out "\n" #:left? #f) "\n" #:trim? #f))

;;================================================
;; The check

(define (env-count default)
  (define v (getenv "HTML_PRINTER_FUZZ_COUNT"))
  (define n (and v (string->number v)))
  (if (exact-nonnegative-integer? n) n default))

(define (env-seed default)
  (define v (getenv "HTML_PRINTER_FUZZ_SEED"))
  (define n (and v (string->number v)))
  (cond [(or (equal? v "random") (equal? default "random")) (random 1 (sub1 (expt 2 31)))]
        [(exact-nonnegative-integer? n) n]
        [else default]))

(define-check (check-fuzz count seed)
  (define n (env-count count))
  (define s (env-seed seed))
  (define print (current-fuzz-printer))
  (parameterize ([current-pseudo-random-generator (make-pseudo-random-generator)])
    (random-seed s)
    (for ([i (in-range n)])
      (define x (random-document))
      (define width (random 6 70))
      (define add-breaks? (zero? (random 2)))
      (define out (print x #:wrap width #:add-breaks? add-breaks?))
      (define (fail! what extra)
        (with-check-info* (append (list (make-check-info 'message (string-info what))
                                        (make-check-info 'seed s)
                                        (make-check-info 'document i)
                                        (make-check-info 'width width)
                                        (make-check-info 'add-breaks? add-breaks?)
                                        (make-check-info 'xexpr x)
                                        (make-check-info 'output (string-info (w/rule width out))))
                                  extra)
                          fail-check))
      (define ours (canonical out))
      (define ref (canonical (xexpr->string x)))
      (unless (equal? ours ref)
        (fail! "output does not render like the input"
               (list (make-check-info 'ours (string-info ours))
                     (make-check-info 'reference (string-info ref)))))
      (for ([line (in-list (output-lines out))])
        (cond
          [(regexp-match? #px"\\s$" line)
           (fail! "line ends in whitespace" (list (make-check-info 'line (string-info line))))]
          [(and (not add-breaks?) (equal? line ""))
           (fail! "empty line" '())]
          [(and (> (string-grapheme-count line) width) (breakable-whitespace? line))
           (fail! "line longer than the wrap width contains a break opportunity"
                  (list (make-check-info 'line (string-info line))))])))))
