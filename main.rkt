#lang racket/base

(require "private/html-tags.rkt"
         "private/log.rkt"
         "private/strings.rkt"
         "private/printer.rkt"
         racket/match
         racket/port
         xml)

(provide xexpr->html5)

;; An attribute is a two-element list of a symbol followed by a string
(define (attr? v)
  (and (list? v)
       (symbol? (car v))
       (not (null? (cdr v)))
       (null? (cddr v))
       (string? (cadr v))))

(define (safe-rest lst)
  (or (and (not (null? lst))
           (cdr lst))
      '()))

;; Given a list of attributes, return a flat list of strings for each attribute name
;; and value. Each string ("chunk") in the list is considered a discrete “word” for
;; line-wrapping purposes
(define (attr-chunks attrs)
  (for/fold ([chunks '()]
             [remaining (safe-rest attrs)]
             #:result (reverse chunks))
            ([attr (in-list attrs)])
    (match-define (list key val) attr)
    (define-values (key-str val-str)
      (cond
        [(boolean-attr? key)
         (values (format (if (null? remaining) "~a>" "~a ") key)
                 "")]
        [else
         (values (format "~a=" key)
                 (format (if (null? remaining) "\"~a\">" "\"~a\" ") (escape val attribute-table)))]))
    (values (cons val-str (cons key-str chunks))
            (safe-rest remaining))))

(module+ test
  (require rackunit)
  (check-equal? (attr-chunks '([href "a"] [title "x"]))
                '("href=" "\"a\" " "title=" "\"x\">")
                "attribute keys/vals correctly formatted as strings")
  (check-equal? (attr-chunks '([href "a"] [selected "x"]))
                '("href=" "\"a\" " "selected>" "")
                "boolean attributes handled correctly (when last attribute)")
  (check-equal? (attr-chunks '([selected "x"] [href "a"]))
                '("selected " "" "href=" "\"a\">")
                "boolean attributes handled correctly (when not last attribute)")
  (check-equal? (attr-chunks '([SELECTED "x"] [href "a"]))
                '("SELECTED " "" "href=" "\"a\">")
                "boolean attributes handled correctly regardless of case"))

(define (opener tag attrs)
  (format (if (not (null? attrs)) "<~a " "<~a>") tag))

(define (closer tag)
  (if (self-closing? tag) "" (format "</~a>" tag)))

;;  'normal = inserting a line break before next element is allowed
;;  'sticky = inserting a line break before next element is not allowed
;;  'flow-opened = a line break+bump was already added directly after previous value
;;  'flow-closed = a line break+unbump was already added directly after previous value
;;  'block-closed = a line break was already added directly after the previous value

(define (normal? v) (eq? v 'normal))
(define (sticky? v) (eq? v 'sticky))
(define (flow-opened? v) (eq? v 'flow-opened))
(define (flow-closed? v) (eq? v 'flow-closed))
(define (block-closed? v) (eq? v 'block-closed))

(define (display-val yeet! v [prev-token 'normal] #:in-inline? [inside-inline? #f])
  (match v
    [(? null?) prev-token]
    [(list* (and (? symbol?) (? br?)) _)
     (log-expr break-tag found v prev-token)
     (yeet! '(put/wrap "<br>") 'flush 'break/indent)
     'flow-opened] ; hacky
    
    ; flow tag
    [(list (? flow? tag) (list (? attr? attrs) ...) elems ...)
     (log-expr flow starting… tag prev-token)
     (when inside-inline?
       (log-err "Block tag ~a inside inline tag ~a; formatting busted!" tag inside-inline?))
     (unless (flow-opened? prev-token) (yeet! 'indent))
     (yeet! `(put ,(opener tag attrs)))
     (for ([a (in-list (attr-chunks attrs))])
       (yeet! `(put/wrap ,a)))
     (yeet! 'flush 'break/++indent)
     (define last-tok
       (for/fold ([last-token 'flow-opened])
                 ([elem (in-list elems)])
         (display-val yeet! elem last-token)))
     (if (or (block-closed? last-tok) (flow-closed? last-tok))
         (yeet! '--indent)
         (yeet! 'flush 'break/--indent))
     (log-expr flow …closing tag last-tok)
     (yeet! `(put/wrap ,(closer tag))
            'break)
     'flow-closed]
    
    ; block tag
    [(list (? block? tag) (list (? attr? attrs) ...) elems ...)
     (log-expr block starting… tag prev-token)
     (when inside-inline?
       (log-err "Block tag ~a inside inline tag ~a; formatting busted!" tag inside-inline?))
     (unless (flow-opened? prev-token) (yeet! 'indent))
     (yeet! `(put ,(opener tag attrs)))
     (for ([a (in-list (attr-chunks attrs))])
       (yeet! `(put/wrap ,a)))   
     (define last-tok
       (for/fold ([last-token 'normal])
                 ([elem (in-list elems)])
         (display-val yeet! elem last-token)))
     (log-expr block …closing tag last-tok)
     (yeet! 'check/flush `(put ,(closer tag)) 'break)
     'block-closed]
    
    ; script, style, pre
    [(list (? preserve-contents? tag) (list (? attr? attrs) ...) elems ...)
     (log-expr preserve start… tag prev-token)
     (unless (flow-opened? prev-token) (yeet! 'indent))
     (yeet! `(put ,(opener tag attrs)))
     (for ([a (in-list (attr-chunks attrs))])
       (yeet! `(put/wrap ,a)))
     (case tag
       [(script style)
        (for ([elem (in-list elems)]) (yeet! `(put ,elem)))]
       [(pre)
        (for ([elem (in-list elems)])
          (yeet! `(put ,(if (string? elem) (escape elem string-element-table) elem))))])
     (log-expr preserve …closing tag)
     (yeet! 'indent-if-col1
            `(put ,(closer tag))
            'break)
     'block-closed]
    
    ; inline tag
    [(list (? symbol? tag) (list (? attr? attrs) ...) elems ...)
     (log-expr inline start… tag prev-token)
     (when (or (flow-closed? prev-token)
               (block-closed? prev-token))
       (yeet! 'indent))
     (yeet! `(,(if (sticky? prev-token) 'put 'put/wrap) ,(opener tag attrs)))
     (for ([a (in-list (attr-chunks attrs))])
       (yeet! `(put/wrap ,a)))
     (define last-token
       (for/fold ([last 'sticky]) ; was prev-token 
                 ([elem (in-list elems)])
         (display-val yeet! elem last #:in-inline? tag)))
     (define popped (yeet! 'pop-whitespace))
     (log-expr inline after popped tag)
     (log-expr inline …closing tag last-token)
     (yeet! `(,(if (sticky? last-token) 'put 'put/wrap) ,(closer tag)))
     (when popped (yeet! `(put/wrap ,popped)))
     (if (member last-token '(normal sticky))
         last-token
         'normal)]
    
    ; no attributes = send it through again
    [(list* (? symbol? tag) elems)
     (display-val yeet! `(,tag () ,@elems) prev-token #:in-inline? inside-inline?)]

    ;; Strings are broken up and printed one word at a time. Much depends on the behavior of
    ;; in-words, which yields separate words for consecutive whitespace and for each individually
    ;; valid combination of CRLF characters (so "\r\r\n" becomes '("\r" "\r\n"), e.g.)
    ;; This match is never reached while inside <script>, <style> or <pre>
    [(? string? str)
     (log-expr string starting… prev-token str)
     (define-values (last-word count)
       (for/fold ([last ""]
                  [count 0]
                  [prev-tok prev-token]
                  #:result (values last count))
                 ([word (in-list (words str))])
         (define out-str (if (linebreak? word) " " (escape word string-element-table)))
         (yeet! `(,(if (sticky? prev-tok) 'put 'put/wrap) ,out-str))
         (values out-str (+ 1 count) 'normal)))
     (log-expr string end last-word)
     (cond
       [(and (memq prev-token '(flow-opened flow-closed block-closed))
             (whitespace? str))
        prev-token]
       [(or (not (whitespace? last-word)) (= 1 count)) 'sticky]
       [else 'normal])]
    
    [(? comment? c)
     (log-expr comment start prev-token)
     (yeet! '(put/wrap "<!--"))
     (for ([str (in-list (words (comment-text c)))])
       (yeet! `(put/wrap ,str)))
     (log-expr comment …closing prev-token)
     (yeet! '(put/wrap "-->") 'flush)
     (when (memq prev-token '(flow-opened flow-closed block-closed))
       (yeet! 'break/indent))
     prev-token]

    [(or (? symbol? v)
         (? exact-positive-integer? v)
         (? xexpr? v))
     (log-expr 'other found v prev-token)
     (yeet! `(,(if (sticky? prev-token) 'put 'put/wrap) ,v))
     (if (or (symbol? v) (number? v)) 'sticky prev-token)]
    [_
     (raise-arguments-error 'xexpr->html5
                            "not a valid element (= txexpr, string, symbol, character integer, CDATA, or comment"
                            "value" v)]
    ))

(define (xexpr->html5 v #:wrap [wrap 100])
  (with-output-to-string
    (λ ()
      (define proc (make-wrapping-printer #:wrap-at wrap))
      (display-val proc v)
      (proc 'flush))))