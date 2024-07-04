#lang racket/base

(require "private/log.rkt"
         "private/strings.rkt"
         "private/printer.rkt"
         racket/list
         racket/match
         racket/port
         racket/string
         racket/symbol
         unicode-breaks)

(provide xexpr->html5)

(define default-block-tags
  '(address article aside canvas dd dl dt fieldset figcaption
            h1 h2 h3 h4 h5 h6 hgroup hr link noscript output p section title tr td meta
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
(define (preserve-contents? tag) (memq (symbol-downcase tag) '(script style pre)))
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
    [(list (and (? symbol?) (? br?)))
     (log-debug "[prev ~a] br" prev-token)
     (yeet! '(put/wrap "<br>") 'flush 'break/indent)
     'flow-opened] ; hacky
    
    ; flow tag
    [(list (? flow? tag) (list (? attr? attrs) ...) elems ...)
     (log-debug "[prev ~a] In flow tag ~a" prev-token tag)
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
     (log-debug "[last ~a] closing ~a" last-tok tag)
     (yeet! `(put/wrap ,(closer tag))
            'break)
     'flow-closed]
    
    ; block tag
    [(list (? block? tag) (list (? attr? attrs) ...) elems ...)
     (log-debug "[prev ~a] In block tag ~a" prev-token tag)
     (when inside-inline?
       (log-err "Block tag ~a inside inline tag ~a; formatting busted!" tag inside-inline?))
     (unless (flow-opened? prev-token) (yeet! 'indent))
     (yeet! `(put ,(opener tag attrs)))
     (for ([a (in-list (attr-chunks attrs))])
       (yeet! `(put/wrap ,a)))   
     (for/fold ([last-token 'normal])
               ([elem (in-list elems)])
       (display-val yeet! elem last-token))
     (yeet! 'check/flush `(put ,(closer tag)) 'break)
     'block-closed]
    
    ; script, style, pre
    [(list (? preserve-contents? tag) (list (? attr? attrs) ...) elems ...)
     (unless (flow-opened? prev-token) (yeet! 'indent))
     (yeet! `(put ,(opener tag attrs)))
     (for ([a (in-list (attr-chunks attrs))])
       (yeet! `(put/wrap ,a)))
     (for ([elem (in-list elems)])
       (yeet! `(put ,elem)))
     (yeet! 'indent-if-col1
            `(put ,(closer tag))
            'break)
     'block-closed]
    
    ; inline tag
    [(list (? symbol? tag) (list (? attr? attrs) ...) elems ...)
     (log-debug "[prev ~a] In inline tag ~a" prev-token tag)
     (when (or (flow-closed? prev-token)
               (block-closed? prev-token))
       (yeet! 'indent))
     (yeet! `(,(if (sticky? prev-token) 'put 'put/wrap) ,(opener tag attrs)))
     (for ([a (in-list (attr-chunks attrs))])
       (yeet! `(put/wrap ,a)))
     (define last-token
       (for/fold ([last prev-token]) ; was sticky 
                 ([elem (in-list elems)])
         (display-val yeet! elem last #:in-inline? tag)))
     (log-debug "[last ~a] closing ~a" last-token tag)
     (yeet! `(,(if (sticky? last-token) 'put 'put/wrap) ,(closer tag)))
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
     (log-debug "[prev ~a] Start - string content…" prev-token)
     (define-values (last-word count)
       (for/fold ([last ""]
                  [count 0]
                  [prev-tok prev-token]
                  #:result (values last count))
                 ([word (in-words str)])
         (log-debug "[prev ~a] string - word ~v" prev-tok word)
         (define out-str (if (linebreak? word) " " (escape word string-element-table)))
         (yeet! `(,(if (sticky? prev-tok) 'put 'put/wrap) ,out-str))
         (values out-str (+ 1 count) 'normal)))
     (log-debug "[~a] End - string content" last-word)
     (cond
       [(and (memq prev-token '(flow-opened flow-closed block-closed))
             (whitespace? str))
        prev-token]
       [(or (not (whitespace? last-word)) (= 1 count)) 'sticky]
       [else 'normal])]

    [(or (? symbol? v)
         (? exact-positive-integer? v))
     (log-debug "[prev ~a] ~a" prev-token v)
     (yeet! `(,(if (sticky? prev-token) 'put 'put/wrap) ,v))
     'sticky]
    ))

(define (xexpr->html5 v #:wrap [wrap 100])
  (with-output-to-string
    (λ ()
      (define proc (make-wrapping-printer #:wrap-at wrap))
      (display-val proc v)
      (proc 'flush))))

;; 25 → "----|----1----|----2----|"
(define (rule n)
  (list->string
   (for/list ([i (in-range n)])
     (define col (+ 1 i))
     (cond
       [(eqv? 0 (modulo col 10))
        (integer->char (+ 48 (/ col 10)))] ; 48 = #\0
       [(eqv? 0 (modulo col 5))
        #\|]
       [else #\-]))))        

(define (w/rule width str)
  (string-append (sys-newline) (rule (+ width 15)) (sys-newline)
                 (string-replace
                  (string-replace str " " "·")
                  (sys-newline) (string-append "¶" (sys-newline)))))

(define (debug v #:wrap [wrap 20])
  (display (w/rule wrap (logging-to-stderr (lambda () (xexpr->html5 v #:wrap wrap))))))

(module+ test
  (require "private/tidy.rkt"
           racket/string)
  
  ;; Convert output to lists of strings for use in tests
  (define (->strs v) (string-split (xexpr->html5 v #:wrap-at 20) (sys-newline)))

  ;; Nice for visual checks
  (define (disp v)
    (displayln (rule 35))
    (display (xexpr->html5 v #:wrap 20)))

  (define-check (check-fmt width msg xpr strs)
    (define my-result (xexpr->html5 xpr #:wrap width))
    (define standard (string-join strs (sys-newline)))
    (unless (equal? my-result standard)
      (with-check-info (['message (string-info msg)]
                        ['|writer result| (string-info (w/rule width my-result))]
                        ['expected (string-info (w/rule width standard))])
        (fail-check))))

  ;; Check the writer's result against the output of HTML Tidy.
  ;; If a sufficiently new version of HTML Tidy is not installed, check passes.
  (define-check (check-matches-tidy? width x)
    (when (tidy-version-sufficient?)
      (define my-result (xexpr->html5 (xpr x) #:wrap width))
      (define tidy-result (string-append (tidy x #:wrap width) "\n"))
      (unless (equal? my-result tidy-result)
        (with-check-info (['message (string-info "writer result does not match expected tidy output")]
                          ['|writer result| (string-info (w/rule width my-result))]
                          ['|tidy output| (string-info (w/rule width tidy-result))])
          (fail-check)))))
  
  (if (tidy-version-sufficient?)
    (eprintf "Test harness using HTML Tidy version ~a for comparison checks\n" (get-tidy-version))
    (eprintf "No stable release of HTML Tidy >= ~a found, skipping Tidy comparison checks\n"
             minimum-tidy-version))

  (check-fmt 20 "Naked strings work correctly" " Hi" '("Hi"))
  
  (check-fmt 20 "simple xexprs wrap correctly"
             (xpr '(p (em "Hello") " World"))
             '("<body>"
               "  <main>"
               "    <article>"
               "      <p>"
               "      <em>Hello</em>"
               "      World</p>"
               "    </article>"
               "  </main>"
               "</body>\n"))

  (check-matches-tidy? 20 '(p (em "Hello") " World"))

  (check-fmt 20 "attribute values never wrap"
             '(head (link [[href "x x x x x x x x x x x x x x x x x x x x"]]))
             '("<head>"
               "  <link href="
               "  \"x x x x x x x x x x x x x x x x x x x x\">"
               "</head>\n"))

  (check-matches-tidy? 20 '(p [[title "x x x x x x x x x x x x x x x x x x x x"]] "z"))

  (check-fmt 20 "UTF-8: Multi-byte emojis count as 1 grapheme and as individual words"
             (xpr '(p "🧞‍♀️🧝‍♂️🧝‍♀️🧙🏽‍♂️🧚🏻🧟‍♂️🧜🏽‍♀️🧞‍♀️🧝‍♂️🧝‍♀️🧙🏽‍♂️🧚🏻🧟‍♂️🧜🏽‍♀️"))
             '("<body>"
               "  <main>"
               "    <article>"
               "      <p>🧞‍♀️🧝‍♂️🧝‍♀️🧙🏽‍♂️🧚🏻🧟‍♂️🧜🏽‍♀️🧞‍♀️🧝‍♂️🧝‍♀️"
               "      🧙🏽‍♂️🧚🏻🧟‍♂️🧜🏽‍♀️</p>"
               "    </article>"
               "  </main>"
               "</body>\n"))

  ;(check-matches-tidy? 20 '(p "🧞‍♀️🧝‍♂️🧝‍♀️🧙🏽‍♂️🧚🏻🧟‍♂️🧜🏽‍♀️🧞‍♀️🧝‍♂️🧝‍♀️🧙🏽‍♂️🧚🏻🧟‍♂️🧜🏽‍♀️"))

  ;; http://utf8everywhere.org — section 8.3
  (check-fmt 20 "UTF-8: Languages with multi-byte graphemes wrap correctly"
             (xpr '(p "Приве́т नमस्ते שָׁלוֹם"))
             '("<body>"
               "  <main>"
               "    <article>"
               "      <p>Приве́т"
               "      नमस्ते שָׁלוֹם</p>"
               "    </article>"
               "  </main>"
               "</body>\n"))

  ;(check-matches-tidy? 20 '(p "Приве́т नमस्ते שָׁלוֹם"))
  ; Block and inline elements as siblings

  (check-fmt 20 "Escape < > & in string elements, and < > & \" in attributes"
             '(span [[x "<\">&"]] "Symbols < \" > &")
             '("<span x="
               "\"&lt;&quot;&gt;&amp;\">"
               "Symbols &lt; \" &gt;"
               "&amp;</span>"))
  
  #;(check-matches-tidy? 20 '(span [[x "<\">&"]] "Symbols < \" > &"))

  (check-fmt 20 "Symbols and numbers converted to entities"
             '(span (em "abcde fghi") nbsp 20)
             '("<span><em>abcde"
               "fghi</em>&nbsp;&#20;</span>"))

  ;(check-matches-tidy? 20 '(p (span (em "abcde fghi") nbsp 20)))

  ; Behavior when indent levels pass wrapping width??
  
  (check-fmt 20 "Linebreaks added after <br>"
             '(p "one" (br) "two three four five six")
             '("<p>one<br>"
               "two three four five"
               "six</p>\n"))

  #;(check-matches-tidy? 20 '(p "one" (br) "two three four five six"))

  (check-fmt 20 "linebreaks not inserted where they would introduce whitespace (following inline tag close)"
             (xpr '(p (em "Hello") "World again"))
             '("<body>"
               "  <main>"
               "    <article>"
               "      <p>"
               "      <em>Hello</em>World"
               "      again</p>"
               "    </article>"
               "  </main>"
               "</body>\n"))

  (check-fmt 20 "linebreaks not inserted where they would introduce whitespace (tag close with only 1 element)"
             (xpr '(p [[class "x x x x x x x x x x x x x x x x x x x x"]] "hi"))
             '("<body>"
               "  <main>"
               "    <article>"
               "      <p class="
               "      \"x x x x x x x x x x x x x x x x x x x x\">"
               "      hi</p>"
               "    </article>"
               "  </main>"
               "</body>\n"))

  (check-matches-tidy? 20 '(p [[class "x x x x x x x x x x x x x x x x x x x x"]] "hi"))

  (check-fmt 20 "linebreaks not inserted where they would introduce whitespace (between inline tags)"
             '(article (p (em "1") (em "2") (em "3") (em "4") (em "5") (em "6")))
             '("<article>"
               "  <p>"
               "  <em>1</em><em>2</em><em>3</em><em>4</em><em>5</em><em>6</em></p>"
               "</article>\n"))

  (check-matches-tidy? 20 '(p (em "1") (em "2") (em "3") (em "4") (em "5") (em "6")))

  (check-fmt 20 "linebreaks not inserted where they would introduce whitespace (final word of inline tag)"
             '(p (span "one two three") (i "four"))
             '("<p><span>one two"
               "three</span><i>four</i></p>\n"))

  (check-fmt 20 "linebreaks present in string element content are preserved as whitespace"
             '(p "What if there are linebreaks\nin the input?")
             '("<p>What if there"
               "are linebreaks in"
               "the input?</p>\n"))

  ;; Not passing yet! requires looking ahead before printing opening <i>
  
  (check-fmt 20 "allow wrap between inline elements when first ends in whitespace"
             '(p (span "one two three ") (i "four"))
             '("<p><span>one two"
               "three </span><i>"
               "four</i></p>\n"))

  #;(check-matches-tidy? 20 '(p (span "one two three ") (i "four")))
  
  (check-fmt 20 "script and style tags are printed without alteration"
             (xpr '(div
                    (script "console.log(5 + 6); console.log(5 + 6); console.log(5 + 6);")
                    (style "body {\n  font-family: serif;\n  color: hsl(var(--accent),10%,35%);\n  padding: env(safe-area-inset-top) env(safe-area-inset-right) env(safe-area-inset-bottom) env(safe-area-inset-left);}")))
             '("<body>"
               "  <main>"
               "    <article>"
               "      <div>"
               "        <script>console.log(5 + 6); console.log(5 + 6); console.log(5 + 6);</script>"
               "        <style>body {"
               "  font-family: serif;"
               "  color: hsl(var(--accent),10%,35%);"
               "  padding: env(safe-area-inset-top) env(safe-area-inset-right) env(safe-area-inset-bottom) env(safe-area-inset-left);}</style>"
               "      </div>"
               "    </article>"
               "  </main>"
               "</body>\n"))

  (check-fmt 20 "script and style closing tags are indented properly when content ends on new line"
             (xpr '(div (script "console.log(5 + 6); console.log(5 + 6); console.log(5 + 6);\n")
                        (style "\nbody {\n  font-family: serif;\n  color: hsl(var(--accent),10%,35%);\n  padding: env(safe-area-inset-top) env(safe-area-inset-right) env(safe-area-inset-bottom) env(safe-area-inset-left);\n}\n")))
             '("<body>"
               "  <main>"
               "    <article>"
               "      <div>"
               "        <script>console.log(5 + 6); console.log(5 + 6); console.log(5 + 6);"
               "        </script>"
               "        <style>"
               "body {"
               "  font-family: serif;"
               "  color: hsl(var(--accent),10%,35%);"
               "  padding: env(safe-area-inset-top) env(safe-area-inset-right) env(safe-area-inset-bottom) env(safe-area-inset-left);"
               "}"
               "        </style>"
               "      </div>"
               "    </article>"
               "  </main>"
               "</body>\n"))

  (check-fmt 20 "Pre tag contents preserved"
             '(div (pre "\none\n  two  \n    three"))
             '("<div>"
               "  <pre>"
               "one"
               "  two  "
               "    three</pre>"
               "</div>\n"))

  (check-fmt 20 "Symbols and integers inside pre/script converted to entities"
             '(div (pre mdash 20))
             '("<div>"
               "  <pre>&mdash;&#20;</pre>"
               "</div>\n"))
  
  (check-fmt 20 "<head> wraps/indents correctly"
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
               "    <title>"
               "    onetwothreefour"
               "    five</title>"
               "  </head>"
               "</html>\n"))

  (check-fmt 20 "Tables wrap as expected"
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

  (check-fmt 80
             "Indentation consistent in the presence of additional whitespace elements"
             '(html ()
                    "\n  "
                    (head () "\n    "
                          (title () "Minimal Post")
                          "\n  ")
                    "\n  "
                    (body () "\n    "
                          (div ((class "article"))
                               (article (h1 "Minimal Post")
                                        (p "Lorem ipsum dolor sit amet, consectetur adipiscing elit. "
                                           "Maecenas nisi libero,\nscelerisque vel nulla eu, "
                                           "tristique porta metus.")))
                          "\n  ")
                    "\n")
             '("<html>"
               "  <head>"
               "    <title>Minimal Post</title>"
               "  </head>"
               "  <body>"
               "    <div class=\"article\">"
               "      <article>"
               "        <h1>Minimal Post</h1>"
               "        <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Maecenas"
               "        nisi libero, scelerisque vel nulla eu, tristique porta metus.</p>"
               "      </article>"
               "    </div>"
               "  </body>"
               "</html>\n"))

  (check-fmt 80
             "Forms wrap as expected"
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
  #;(debug '(body (p (em "Hello " (div "World")) "woah")))
  
  ; self-closing tags display properly
  (for ([tag (in-list '(area base basefont br col frame hr img input isindex link meta param))])
    (check-fmt 20
               (format "~a displays correctly as self-closing tag" tag)
               `(,tag)
               (list (format "<~a>~a"
                             tag
                             (if (or (block? tag) (flow? tag) (eq? tag 'br)) (sys-newline) "")))))
  )