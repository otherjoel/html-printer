#lang racket/base

(require "private/html-tags.rkt"
         "private/log.rkt"
         "private/strings.rkt"
         "private/printer.rkt"
         racket/match
         racket/port
         racket/string
         xml)

(provide xexpr->html5)

;; An attribute is a two-element list of a symbol followed by a string
(define (attr? v)
  (and (list? v)
       (symbol? (car v))
       (not (null? (cdr v)))
       (null? (cddr v))
       (string? (cadr v))))

;; An opening tag with its attributes, as a list of printer tokens. The tag name and the first
;; attribute name are glued together (as in `<a href=`), as HTML Tidy does; each following
;; attribute may start a new line, and each attribute value may start a new line after its `=`.
;; Attribute values themselves never wrap. The last chunk carries the closing `>`.
(define (opener-tokens tag attrs)
  (define (attr-tokens attr first? last?)
    (match-define (list key val) attr)
    (define lead (if first? (format "<~a " tag) ""))
    (define end (if last? ">" ""))
    (define sep (if first? '() (list 'space)))
    (cond
      [(boolean-attr? key)
       (append sep (list (text (format "~a~a~a" lead key end))))]
      [else
       (append sep (list (text (format "~a~a=" lead key))
                         'softbreak
                         (text (format "\"~a\"~a"
                                       (escape (string-normalize-spaces val) attribute-table)
                                       end))))]))
  (cond
    [(null? attrs) (list (text (format "<~a>" tag)))]
    [else
     (define last-i (- (length attrs) 1))
     (apply append
            (for/list ([attr (in-list attrs)] [i (in-naturals)])
              (attr-tokens attr (= i 0) (= i last-i))))]))

(module+ test
  (require rackunit)
  (check-equal? (opener-tokens 'p '())
                (list (text "<p>"))
                "no attributes")
  (check-equal? (opener-tokens 'a '([href "a"] [title "x"]))
                (list (text "<a href=") 'softbreak (text "\"a\"")
                      'space (text "title=") 'softbreak (text "\"x\">"))
                "attribute keys/vals correctly formatted as tokens")
  (check-equal? (opener-tokens 'a '([href "a"] [selected "x"]))
                (list (text "<a href=") 'softbreak (text "\"a\"") 'space (text "selected>"))
                "boolean attributes handled correctly (when last attribute)")
  (check-equal? (opener-tokens 'a '([selected "x"] [href "a"]))
                (list (text "<a selected") 'space (text "href=") 'softbreak (text "\"a\">"))
                "boolean attributes handled correctly (when not last attribute)")
  (check-equal? (opener-tokens 'a '([SELECTED "x"] [href "a"]))
                (list (text "<a SELECTED") 'space (text "href=") 'softbreak (text "\"a\">"))
                "boolean attributes handled correctly regardless of case")
  (check-equal? (opener-tokens 'p '([title "a \n b"]))
                (list (text "<p title=") 'softbreak (text "\"a b\">"))
                "whitespace in attribute values is normalized"))

(define (closer tag)
  (if (self-closing? tag) "" (format "</~a>" tag)))

;; Walk an X-expression, producing the list of printer tokens that lays it out. See printer.rkt
;; for the token vocabulary. The layout rules, in brief:
;;
;;  • Flow tags (article, ul, li, …) start on their own line; their contents are indented one
;;    level and start on a new line; the closing tag gets its own line.
;;  • Block tags (p, h1, td, …) start on their own line, but their contents are laid out inline
;;    and the closing tag is glued to the last piece of content.
;;  • Inline tags and strings are laid out inline. Lines may break only where the input has
;;    whitespace (or where added whitespace is insignificant, such as right after a block's
;;    opening tag), so the output never changes the meaning of the input.
;;  • The contents of <pre>, <script> and <style> are printed verbatim.
(define (xexpr->tokens v add-breaks?)
  (define acc '()) ; tokens so far, most recent first
  (define (emit! . toks)
    (for ([t (in-list toks)]) (set! acc (cons t acc))))
  (define (after-space?) (and (pair? acc) (eq? (car acc) 'space)))
  (define (pop-space!)
    (cond [(after-space?) (set! acc (cdr acc)) #t]
          [else #f]))
  (define (pop-softbreak!)
    (when (and (pair? acc) (eq? (car acc) 'softbreak)) (set! acc (cdr acc))))

  ;; With #:add-breaks?, an empty line separates a closed block/flow tag from the next tag
  (define (maybe-blank! tag prev-block?)
    (when (and add-breaks? prev-block? (not (memq (symbol-downcase tag) '(meta link title))))
      (emit! 'blank)))

  (define (walk-children elems parent)
    (for/fold ([prev-block? #f]) ([elem (in-list elems)])
      (walk elem parent prev-block?)))

  ;; parent is the kind of the enclosing element: 'top, 'flow, 'block or 'inline.
  ;; prev-block? is #t if the previous sibling ended with a hard line break.
  ;; Returns the value of prev-block? for the next sibling.
  (define (walk v parent prev-block?)
    (match v
      [(? null?) prev-block?]

      [(list* (and (? symbol?) (? br?)) _)
       (log-expr break-tag found v)
       (emit! 'softbreak (text "<br>") 'newline)
       #f]

      ; flow tag
      [(list (? flow? tag) (list (? attr? attrs) ...) elems ...)
       (log-expr flow starting… tag parent prev-block?)
       (maybe-blank! tag prev-block?)
       ;; A flow tag inside a block or inline tag (bad HTML, but tolerated) is indented relative
       ;; to the line holding its parent
       (define nested? (memq parent '(block inline)))
       (when nested?
         (log-expr flow "inside block/inline tag (considered weird)" tag)
         (emit! 'indent+))
       (emit! 'newline)
       (apply emit! (opener-tokens tag attrs))
       (emit! 'indent+ 'newline)
       (walk-children elems 'flow)
       (log-expr flow …closing tag)
       (emit! 'indent- 'newline (text (closer tag)) 'newline)
       (when nested? (emit! 'indent-))
       #t]

      ; block tag
      [(list (? block? tag) (list (? attr? attrs) ...) elems ...)
       (log-expr block starting… tag parent prev-block?)
       (maybe-blank! tag prev-block?)
       (when (eq? parent 'inline)
         (log-expr block "inside inline tag (considered weird)" tag))
       (emit! 'newline)
       (apply emit! (opener-tokens tag attrs))
       (emit! 'softbreak)
       (walk-children elems 'block)
       ;; An empty block keeps its closing tag glued to the opening tag
       (pop-softbreak!)
       (log-expr block …closing tag)
       (emit! (text (closer tag)) 'newline)
       #t]

      ; script, style, pre: contents printed verbatim (escaped in the case of pre)
      [(list (? preserve-contents? tag) (list (? attr? attrs) ...) elems ...)
       (log-expr preserve starting… tag parent prev-block?)
       (maybe-blank! tag prev-block?)
       (emit! 'newline)
       (apply emit! (opener-tokens tag attrs))
       (define pre? (eq? (symbol-downcase tag) 'pre))
       (emit! (raw (apply string-append
                          (for/list ([elem (in-list elems)])
                            (if (and pre? (string? elem))
                                (escape elem string-element-table)
                                (->string elem))))
                   #f))
       (log-expr preserve …closing tag)
       ;; When the content ends with a newline, </script> and </style> are indented, but </pre>
       ;; goes at column 1: any indent there would be part of the preformatted content
       (emit! (raw (closer tag) (not pre?)) 'newline)
       #t]

      ; inline tag
      [(list (? symbol? tag) (list (? attr? attrs) ...) elems ...)
       (log-expr inline starting… tag parent prev-block?)
       (maybe-blank! tag prev-block?)
       (apply emit! (opener-tokens tag attrs))
       (walk-children elems 'inline)
       ;; Trailing whitespace inside an inline tag is moved after the closing tag, so that
       ;; <em>word </em> becomes <em>word</em> (and the closing tag stays glued to the word)
       (define popped? (pop-space!))
       (log-expr inline …closing tag popped?)
       (emit! (text (closer tag)))
       (when popped? (emit! 'space))
       #f]

      ; no attributes = send it through again
      [(list* (? symbol? tag) elems)
       (walk `(,tag () ,@elems) parent prev-block?)]

      ;; Strings are split into words and whitespace; a line may break at any whitespace
      [(? string? str)
       (log-expr string found parent prev-block? str)
       (for ([word (in-list (words str))])
         (emit! (if (whitespace? word) 'space (text (escape word string-element-table)))))
       (if (whitespace? str) prev-block? #f)]

      ;; Comments are wrapped like text. A comment that starts a line (first thing after a flow tag
      ;; opens, or after a block-level sibling) gets its own line(s); anywhere else it is inline.
      [(? comment? c)
       (log-expr comment found parent prev-block?)
       (define own-line? (and (pair? acc) (eq? (car acc) 'newline)))
       ;; A line break directly after a comment adds no significant whitespace if there was
       ;; already whitespace directly before it (the two collapse together)
       (define breakable-after? (and (not own-line?) (after-space?)))
       (when own-line? (emit! 'newline))
       (emit! (text "<!--"))
       (for ([word (in-list (words (comment-text c)))])
         (emit! (if (whitespace? word) 'space (text word))))
       (emit! (text "-->"))
       (when own-line? (emit! 'newline))
       (when breakable-after? (emit! 'softbreak))
       prev-block?]

      [(or (? symbol? v) (? exact-positive-integer? v))
       (log-expr entity found v)
       (emit! (text (->string v)))
       #f]

      ;; Anything else (CDATA, processing instructions…) is printed as-is, as one unbreakable
      ;; chunk. As with comments, a break directly after it is harmless if whitespace preceded it.
      [(? xexpr? v)
       (log-expr other found v)
       (define breakable-after? (after-space?))
       (emit! (text (->string v)))
       (when breakable-after? (emit! 'softbreak))
       prev-block?]

      [_
       (raise-arguments-error 'xexpr->html5
                              "not a valid element (= txexpr, string, symbol, character integer, CDATA, or comment"
                              "value" v)]))

  (walk v 'top #f)
  (reverse acc))

(define (xexpr->html5 v #:wrap [wrap 100] #:add-breaks? [add-breaks? #f])
  (with-output-to-string
    (λ ()
      (when (and (pair? v) (symbol? (car v)) (eq? 'html (symbol-downcase (car v))))
        (displayln "<!DOCTYPE html>"))
      (define print! (make-wrapping-printer #:wrap-at wrap))
      (print! (xexpr->tokens v add-breaks?) 'flush))))
