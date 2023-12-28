#lang racket/base

(require "private/log.rkt"
         "private/strings.rkt"
         "private/printer.rkt"
         racket/class
         racket/match
         racket/port
         racket/symbol
         unicode-breaks)

(define default-block-tags
  '(address article aside canvas dd dl dt fieldset figcaption
            h1 h2 h3 h4 h5 h6 hgroup hr link noscript output p pre section title tr td meta
            video))

(define default-flow-tags
  '(article blockquote body div figure footer form html head header li main nav ol section table tbody
            thead tfoot tr ul))

(define default-selfclose-tags
  '(area base basefont br col frame hr img input isindex link meta param))

;; https://html.spec.whatwg.org/multipage/indices.html#attributes-3
(define html5-boolean-attrs
  '(allowfullscreen async autofocus autoplay checked controls default defer disabled formnovalidate
                    inert ismap itemscope loop multiple muted nomodule novalidate open playsinline
                    readonly required reversed selected))

(define (symbol-downcase sym)
  (string->symbol (string-downcase (symbol->immutable-string sym))))

(define (block? tag) (memq (symbol-downcase tag) default-block-tags))
(define (flow? tag) (memq (symbol-downcase tag) default-flow-tags))
(define (script-or-style? tag) (memq (symbol-downcase tag) '(script style)))
(define (self-closing? tag) (memq (symbol-downcase tag) default-selfclose-tags))
(define (boolean-attr? a) (memq (symbol-downcase a) html5-boolean-attrs))
(define (br? tag) (eq? (symbol-downcase tag) 'br))

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
         (values (format (if (null? remaining) "~a>" "~a ") (symbol->immutable-string key))
                 "")]
        [else
         (values (format "~a=" key)
                 (format (if (null? remaining) "\"~a\">" "\"~a\" ") val))]))
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

(define (display-val hp v [prev-token 'normal] #:in-inline? [inside-inline? #f])
  (match v
    [(list (and (? symbol?) (? br?)))
     (send hp put/wrap! "<br>")
     (send hp break/indent!)
     'flow-opened] ; hacky
    ; flow tag
    [(list (? flow? tag) (list (? attr? attrs) ...) elems ...)
     (log-debug "In flow tag ~a" tag)
     (when inside-inline?
       (log-err "Flow tag ~a inside inline tag ~a; formatting will be incorrect!" tag inside-inline?))
     (unless (flow-opened? prev-token) (send hp indent!))
     (send hp put! (opener tag attrs))
     (unless (null? attrs) (log-debug "Before ~a attrs, at column ~a" tag (send hp get-col)))
     (for ([a (in-list (attr-chunks attrs))])
       (send hp put/wrap! a))
     (unless (null? attrs) (log-debug "After ~a attrs, at column ~a" tag (send hp get-col)))
     (send hp break+bump!)
     (define last-tok
       (for/fold ([last-token 'flow-opened])
                 ([elem (in-list elems)])
         (display-val hp elem last-token)))
     (if (or (block-closed? last-tok) (flow-closed? last-tok))
         (send hp indent/unbump!)
         (send hp break+unbump!))
     (send hp put/wrap! (closer tag))
     (send hp break!)
     'flow-closed]
    
    ; block tag
    [(list (? block? tag) (list (? attr? attrs) ...) elems ...)
     (log-debug "In block tag ~a" tag)
     (cond
       [inside-inline?
        (log-err "Block tag ~a inside inline tag ~a; formatting will be incorrect!"
                 tag
                 inside-inline?)])
     (unless (flow-opened? prev-token) (send hp indent!))
     (unless (null? attrs) (log-debug "Before ~a attrs, at column ~a" tag (send hp get-col)))
     (send hp put! (opener tag attrs))
     (for ([a (in-list (attr-chunks attrs))])
       (send hp put/wrap! a))
     (unless (null? attrs) (log-debug "After ~a attrs, at column ~a" tag (send hp get-col)))
     (for/fold ([last-token 'sticky])
               ([elem (in-list elems)])
       (display-val hp elem last-token))
     (send hp put! (closer tag))
     (send hp break!)
     'block-closed]
    
    ; script or style
    [(list (? script-or-style? tag) (list (? attr? attrs) ...) elems ...)
     (log-debug "In ~a tag" tag)
     (unless (flow-opened? prev-token) (send hp indent!))
     (send hp put! (opener tag attrs))
     (for ([a (in-list (attr-chunks attrs))])
       (send hp put/wrap! a))
     (for ([elem (in-list elems)])
       (log-debug "elem: ~a" elem)
       (send hp put! elem))
     (log-debug "Ended ~a tag content at column ~a" tag (send hp get-col))
     (when (= 1 (send hp get-col)) (send hp indent!))
     (send hp put! (closer tag))
     (send hp break!)
     'block-closed]
    
    ; inline tag
    [(list (? symbol? tag) (list (? attr? attrs) ...) elems ...)
     (log-debug "In inline tag ~a" tag)
     (when (or (flow-closed? prev-token) (block-closed? prev-token)) (send hp indent!))
     (cond [(sticky? prev-token)
            (send hp put! (opener tag attrs))]
           [else
            (send hp put/wrap! (opener tag attrs))])
     (for ([a (in-list (attr-chunks attrs))])
       (send hp put/wrap! a))
     (define last-token
       (for/fold ([last 'sticky])
                 ([elem (in-list elems)])
         (display-val hp elem last #:in-inline? tag)))
     (cond [(sticky? last-token)
            (send hp put! (closer tag))]
           [else
            (send hp put/wrap! (closer tag))])
     last-token]
    
    ; no attributes = send it through again
    [(list* (? symbol? tag) elems)
     (display-val hp `(,tag () ,@elems) prev-token #:in-inline? inside-inline?)]

    ;; Strings are broken up and printed one word at a time. Much depends on the behavior of
    ;; in-words, which yields separate words for consecutive whitespace and for each individually
    ;; valid combination of CRLF characters (so "\r\r\n" becomes '("\r" "\r\n"), e.g.)
    [(? string? str)
     (log-debug "Processing string content…")
     (define-values (last-word count)
       (for/fold ([last ""]
                  [count 0]
                  [prev-tok prev-token]
                  #:result (values last count))
                 ([word (in-words str)])
         (define out-str (if (linebreak? word) "" word))
         (if (sticky? prev-tok)
             (send hp put! out-str)
             (send hp put/wrap! out-str))
         (values out-str (+ 1 count) 'normal)))
     (cond
       [(or (whitespace? last-word) (= 1 count)) 'sticky]
       [else 'normal])]
    ))

(define (xexpr->html5 v #:wrap-at [wrap 100])
  (with-output-to-string
    (λ () (display-val (new wrapping-printer% [wrap wrap]) v))))


(module+ test
  (require (for-syntax racket/base) racket/string)
  (define (->str v) (string-split (xexpr->html5 v #:wrap-at 20) (sys-newline)))
  ;(->str '(main (article (p (em "Hello") " World"))))

  (define-syntax (check-fmt stx)
    (syntax-case stx ()
      [(_ msg xpr strs)
       (syntax/loc stx
         (check-equal? (xexpr->html5 xpr #:wrap-at 20) (string-join strs (sys-newline)) msg))]
      [(_ width msg xpr strs)
       (syntax/loc stx
         (check-equal? (xexpr->html5 xpr #:wrap-at width) (string-join strs (sys-newline)) msg))]))
  
  (define (xpr vs) `(body (main (article ,@vs))))
  ;(define (rule) (displayln "\n123456789012345678980"))

  (check-fmt "Naked strings work correctly" " Hi" '("Hi"))
  
  (check-fmt "simple xexprs wrap correctly"
             (xpr '((p (em "Hello") " World")))
             '("<body>"
               "  <main>"
               "    <article>"
               "      <p><em>Hello</em> "
               "      World</p>"
               "    </article>"
               "  </main>"
               "</body>\n"))

  (check-fmt "attribute values never wrap"
             `(head (link [[href "x x x x x x x x x x x x x x x x x x x x"]]))
             '("<head>"
               "  <link href=" "  \"x x x x x x x x x x x x x x x x x x x x\">"
               "</head>\n"))

  (check-fmt "Linebreaks added after <br>"
             '(p "one" (br) "two three four five six")
             '("<p>one<br>"
               "two three four five"
               "six</p>\n"))

  (check-fmt "linebreaks not inserted where they would introduce whitespace (following inline tag close)"
             (xpr '((p (em "Hello") "World again")))
             '("<body>"
               "  <main>"
               "    <article>"
               "      <p><em>Hello</em>World"
               "      again</p>"
               "    </article>"
               "  </main>"
               "</body>\n"))

  (check-fmt "linebreaks not inserted where they would introduce whitespace (tag close with only 1 element)"
             (xpr '((p [[class "x x x x x x x x x x x x x x x x x x x x"]] "hi")))
             '("<body>"
               "  <main>"
               "    <article>"
               "      <p class="
               "      \"x x x x x x x x x x x x x x x x x x x x\">hi</p>"
               "    </article>"
               "  </main>"
               "</body>\n"))

  (check-fmt "linebreaks not inserted where they would introduce whitespace (between inline tags)"
             '(article (p (em "1") (em "2") (em "3") (em "4") (em "5") (em "6")))
             '("<article>"
               "  <p><em>1</em><em>2</em><em>3</em><em>4</em><em>5</em><em>6</em></p>"
               "</article>\n"))
  
  (check-fmt "script and style tags are printed without alteration"
             (xpr '((script "console.log(5 + 6); console.log(5 + 6); console.log(5 + 6);")
                    (style "body {\n  font-family: serif;\n  color: hsl(var(--accent),10%,35%);\n  padding: env(safe-area-inset-top) env(safe-area-inset-right) env(safe-area-inset-bottom) env(safe-area-inset-left);}")))
             '("<body>"
               "  <main>"
               "    <article>"
               "      <script>console.log(5 + 6); console.log(5 + 6); console.log(5 + 6);</script>"
               "      <style>body {"
               "  font-family: serif;"
               "  color: hsl(var(--accent),10%,35%);"
               "  padding: env(safe-area-inset-top) env(safe-area-inset-right) env(safe-area-inset-bottom) env(safe-area-inset-left);}</style>"
               "    </article>"
               "  </main>"
               "</body>\n"))

  (check-fmt "script and style closing tags are indented properly when content ends on new line"
             (xpr '((script "console.log(5 + 6); console.log(5 + 6); console.log(5 + 6);\n")
                    (style "\nbody {\n  font-family: serif;\n  color: hsl(var(--accent),10%,35%);\n  padding: env(safe-area-inset-top) env(safe-area-inset-right) env(safe-area-inset-bottom) env(safe-area-inset-left);\n}\n")))
             '("<body>"
               "  <main>"
               "    <article>"
               "      <script>console.log(5 + 6); console.log(5 + 6); console.log(5 + 6);"
               "      </script>"
               "      <style>"
               "body {"
               "  font-family: serif;"
               "  color: hsl(var(--accent),10%,35%);"
               "  padding: env(safe-area-inset-top) env(safe-area-inset-right) env(safe-area-inset-bottom) env(safe-area-inset-left);"
               "}"
               "      </style>"
               "    </article>"
               "  </main>"
               "</body>\n"))
  
  ;; Header section 
  (check-fmt "<head> wraps/indents correctly"
             '(html (head (link [[rel "stylesheet"] [href "style.css"]])
                          (meta [[charset "UTF-8"]])
                          (title "onetwothreefour five")))
             '("<html>"
               "  <head>"
               "    <link rel="
               "    \"stylesheet\" "
               "    href="
               "    \"style.css\">"
               "    <meta charset="
               "    \"UTF-8\">"
               "    <title>onetwothreefour"
               "    five</title>"
               "  </head>"
               "</html>\n"))

  (check-fmt "Tables wrap as expected"
             '(table (thead (tr (td "Col 1") (td "Col 2") (td "Col 3")))
                     (tbody (tr (td "a") (td "b") (td "c"))))
             '("<table>"
               "  <thead>"
               "    <tr>"
               "      <td>Col 1</td>"
               "      <td>Col 2</td>"
               "      <td>Col 3</td>"
               "    </tr>"
               "  </thead>"
               "  <tbody>"
               "    <tr>"
               "      <td>a</td>"
               "      <td>b</td>"
               "      <td>c</td>"
               "    </tr>"
               "  </tbody>"
               "</table>\n"))

  (check-fmt 80 "Forms wrap as expected"
             '(form ((accept-charset "utf-8")
                     (action "https://example.com/subscribe")
                     (class "row-form")
                     (method "POST"))
                    (label ((for "name")) "Name") (br)
                    (input ((id "name") (name "name") (type "text"))) (br)
                    (label ((for "email")) "Email") (br)
                    (input ((id "email") (name "email") (type "email"))) (br)
                    (div ((style "display:none;"))
                         (label ((for "hp")) "HP") (br)
                         (input ((id "hp") (name "hp") (type "text"))))
                    (input ((name "list") (type "hidden") (value "xxx")))
                    (input ((name "subform") (type "hidden") (value "yes")))
                    (input ((id "submit") (name "submit") (type "submit") (value "Subscribe"))))
             '("<form accept-charset=\"utf-8\" action=\"https://example.com/subscribe\" class="
               "\"row-form\" method=\"POST\">"
               "  <label for=\"name\">Name</label><br>"
               "  <input id=\"name\" name=\"name\" type=\"text\"><br>"
               "  <label for=\"email\">Email</label><br>"
               "  <input id=\"email\" name=\"email\" type=\"email\"><br>"
               "  <div style=\"display:none;\">"
               "    <label for=\"hp\">HP</label><br>"
               "    <input id=\"hp\" name=\"hp\" type=\"text\">"
               "  </div>"
               "  <input name=\"list\" type=\"hidden\" value=\"xxx\"><input name=\"subform\" type="
               "  \"hidden\" value=\"yes\"><input id=\"submit\" name=\"submit\" type=\"submit\" value="
               "  \"Subscribe\">"
               "</form>\n"))

  ;; Broken HTML, not sure what to do with this
  (define broken '(body (p (em "Hello " (div "World")) "woah")))
  ;(logging-to-stderr (lambda () (display (->str broken))))


  ; self-closing tags display properly
  (for ([tag (in-list '(area base basefont br col frame hr img input isindex link meta param))])
    (check-fmt (format "~a displays correctly as self-closing tag" tag)
               `(,tag)
               (list (format "<~a>~a"
                             tag
                             (if (or (block? tag) (flow? tag) (eq? tag 'br)) (sys-newline) ""))))))