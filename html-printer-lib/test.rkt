#lang racket/base

(require "tests/test-util.rkt")

(module+ test
  (require "main.rkt"
           "tests/fuzz.rkt"
           "private/tidy.rkt"
           "private/html-tags.rkt"
           "private/strings.rkt"
           xml)
  ;;================================================
  ;; Custom element names
  ;; https://html.spec.whatwg.org/multipage/custom-elements.html#custom-elements-core-concepts

  (check-true (valid-custom-element-name? "my-element"))
  (check-true (valid-custom-element-name? 'my-element))
  (check-true (valid-custom-element-name? "my-complex-element")) ; Multiple hyphens
  (check-true (valid-custom-element-name? "my-element-v2"))      ; With numbers
  (check-true (valid-custom-element-name? "my-element_2"))       ; With underscore
  (check-true (valid-custom-element-name? "my-element.2"))       ; With dot
  (check-true (valid-custom-element-name? "element-"))           ; Ends with hyphen
  (check-true (valid-custom-element-name? "element--test"))      ; Double hyphen
  (check-true (valid-custom-element-name? "café-element"))       ; Latin characters with accents (é = \u00E9)
  (check-true (valid-custom-element-name? "naïve-component"))    ; Latin with diacritic (ï = \u00EF)
  (check-true (valid-custom-element-name? "résumé-widget"))      ; Multiple accented characters
  (check-true (valid-custom-element-name? "björn-element"))      ; Swedish characters (ö = \u00F6)
  (check-true (valid-custom-element-name? "element-ñoño"))       ; Spanish ñ character
  (check-true (valid-custom-element-name? "café·bar-element"))   ; With middle dot (\u00B7)
  (check-true (valid-custom-element-name? "greek-φιλοσοφία"))      ; Greek characters
  (check-true (valid-custom-element-name? "japanese-こんにちは"))  ; Japanese hiragana
  (check-true (valid-custom-element-name? "korean-안녕하세요"))    ; Korean characters
  (check-true (valid-custom-element-name? "thai-สวัสดี"))          ; Thai characters
  (check-true (valid-custom-element-name? "element-ṵ"))          ; Latin with combining marks
  (check-true (valid-custom-element-name? "element-🗂️"))        ; Emoji with variation selector
  (check-true (valid-custom-element-name? "element-﷽"))  ; Arabic ligature (might be out of range)

  (check-false (valid-custom-element-name? "element"))           ; No hyphen
  (check-false (valid-custom-element-name? "MyElement"))         ; Uppercase letters
  (check-false (valid-custom-element-name? "my_element"))        ; No hyphen
  (check-false (valid-custom-element-name? "-element"))          ; Starts with hyphen
  (check-false (valid-custom-element-name? "123-element"))       ; Starts with number
  (check-false (valid-custom-element-name? ""))                  ; Empty string
  (check-false (valid-custom-element-name? "my element"))        ; Contains space
  (check-false (valid-custom-element-name? "A-element"))         ; Starts with uppercase
  (check-false (valid-custom-element-name? "my-Element"))        ; Uppercase in second part
  (check-false (valid-custom-element-name? "🙂-element"))        ; Emoji (not in PCENChar ranges)

  (check-true (flow? 'my-element))

  (if (tidy-version-sufficient?)
      (printf "Test harness using HTML Tidy version ~a for comparison checks\n" (get-tidy-version))
      (printf "No stable release of HTML Tidy >= ~a found, skipping Tidy comparison checks\n"
              minimum-tidy-version))

  (check-fmt 20 "Naked strings work correctly" " Hi" '("Hi"))

  (check-exn exn:fail:contract?
             (λ () (xexpr->html5 `(div ,(void))))
             "Invalid X-expression raises an exception")
  
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

  (check-fmt 20 "whitespace/line breaks in attribute values normalized to single spaces"
             '(head (link [[href "x\nx  x \tx \r\n\t x"]]))
             '("<head>"
               "  <link href="
               "  \"x x x x x\">"
               "</head>\n"))

  (check-fmt 20 "UTF-8: Multi-byte emojis count as 1 grapheme"
             (xpr '(p "🧞‍♀️🧝‍♂️🧝‍♀️🧙🏽‍♂️🧚🏻🧟‍♂️🧜🏽‍♀️🧞‍♀️🧝‍♂️ 🧝‍♀️🧙🏽‍♂️🧚🏻🧟‍♂️🧜🏽‍♀️"))
             '("<body>"
               "  <main>"
               "    <article>"
               "      <p>🧞‍♀️🧝‍♂️🧝‍♀️🧙🏽‍♂️🧚🏻🧟‍♂️🧜🏽‍♀️🧞‍♀️🧝‍♂️"
               "      🧝‍♀️🧙🏽‍♂️🧚🏻🧟‍♂️🧜🏽‍♀️</p>"
               "    </article>"
               "  </main>"
               "</body>\n"))

  (check-fmt 20 "UTF-8: Consecutive multi-byte emojis count as 1 word"
             (xpr '(p "🧞‍♀️🧝‍♂️🧝‍♀️🧙🏽‍♂️🧚🏻🧟‍♂️🧜🏽‍♀️🧞‍♀️🧝‍♂️🧝‍♀️🧙🏽‍♂️🧚🏻🧟‍♂️🧜🏽‍♀️"))
             '("<body>"
               "  <main>"
               "    <article>"
               "      <p>"
               "      🧞‍♀️🧝‍♂️🧝‍♀️🧙🏽‍♂️🧚🏻🧟‍♂️🧜🏽‍♀️🧞‍♀️🧝‍♂️🧝‍♀️🧙🏽‍♂️🧚🏻🧟‍♂️🧜🏽‍♀️</p>"
               "    </article>"
               "  </main>"
               "</body>\n"))

  (check-matches-tidy? 20 '(p "🧞‍♀️🧝‍♂️🧝‍♀️🧙🏽‍♂️🧚🏻🧟‍♂️🧜🏽‍♀️🧞‍♀️🧝‍♂️🧝‍♀️🧙🏽‍♂️🧚🏻🧟‍♂️🧜🏽‍♀️")) ; a single word: overflows either way

  ;This check will never succeed because HTML Tidy is not grapheme-aware (as of 5.8.0)
  ; (check-matches-tidy? 20 '(p "Приве́т नमस्ते שָׁלוֹם"))
  
  ;; http://utf8everywhere.org — section 8.3
  (check-fmt 20 "UTF-8: Languages with multi-byte graphemes wrap correctly"
             (xpr '(p "Приве́т سلام नमस्ते שָׁלוֹם"))
             '("<body>"
               "  <main>"
               "    <article>"
               "      <p>Приве́т سلام"
               "      नमस्ते שָׁלוֹם</p>"
               "    </article>"
               "  </main>"
               "</body>\n"))

  (check-fmt 80 "Escape < > & in string elements, and < > & \" in attributes"
             '(span [[x "<\">&"]] "Symbols < \" > &")
             '("<span x=\"&lt;&quot;&gt;&amp;\">Symbols &lt; \" &gt; &amp;</span>"))
  
  ;(check-matches-tidy? 20 '(span [[x "<\">&"]] "Symbols < \" > &"))

  (check-fmt 20 "Symbols and numbers converted to entities"
             '(span (em "abcde fghi") nbsp 20)
             '("<span><em>abcde"
               "fghi</em>&nbsp;&#20;</span>"))

  ;(check-matches-tidy? 20 '(p (span (em "abcde fghi") nbsp 30)))

  ; Behavior when indent levels pass wrapping width??
  
  (check-fmt 20 "Linebreaks added after <br>"
             '(p "one" (br) "two three four five six")
             '("<p>one<br>"
               "two three four five"
               "six</p>\n"))
  
  (check-matches-tidy? 30 '(p "one" (br) "two three four five six"))

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

  (check-matches-tidy? 20 '(p (em "Hello") "World again"))

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

  (check-matches-tidy? 30 (xpr '(p (span "one two three") (i "four"))))

  (check-fmt 20 "linebreaks present in string element content are preserved as whitespace"
             '(p "What if there are linebreaks\nin the input?")
             '("<p>What if there are"
               "linebreaks in the"
               "input?</p>\n"))
  
  (check-fmt 20 "allow wrap between inline elements when first ends in whitespace"
             '(p (span "one two three ") (i "four"))
             '("<p><span>one two"
               "three</span>"
               "<i>four</i></p>\n"))

  (check-matches-tidy? 20 '(p (span "one two three ") (i "four")))
  
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

  (check-fmt 20 "No escaping inside script and style tags"
             '(div (script "/* < > & */") (style "/* < > & */"))
             '("<div>"
               "  <script>/* < > & */</script>"
               "  <style>/* < > & */</style>"
               "</div>\n"))

  (check-fmt 20 "Pre tag contents preserved"
             '(div (pre "\none\n  two  \n    three"))
             '("<div>"
               "  <pre>"
               "one"
               "  two  "
               "    three</pre>"
               "</div>\n"))

  (check-matches-tidy? 20 '(div (pre "\none\n  two  \n    three")))

  (check-fmt 20 "Pre tags handle contained xexprs"
             '(div (pre "Hello " (i "World")))
             '("<div>"
               "  <pre>Hello <i>World</i></pre>"
               "</div>\n"))

  (check-matches-tidy? 20 '(div (pre "Hello " (i "World"))))

  (check-fmt 20 "Special characters inside pre tags escaped"
             '(div (pre "< > &"))
             '("<div>"
               "  <pre>&lt; &gt; &amp;</pre>"
               "</div>\n"))

  (check-fmt 20 "Symbols and integers inside pre/script converted to entities"
             '(div (pre mdash 20))
             '("<div>"
               "  <pre>&mdash;&#20;</pre>"
               "</div>\n"))

  (check-fmt 20 "Handle empty attribute lists, esp. in <br> tags (Koyo/HAML)"
             '(body ()
                    (header ()
                            (h1 () "Title")
                            (p () "a"
                               (br ())
                               "b")))
             '("<body>"
               "  <header>"
               "    <h1>Title</h1>"
               "    <p>a<br>"
               "    b</p>"
               "  </header>"
               "</body>\n"))

  (check-matches-tidy? 20 '(body () (header () (h1 () "Title") (p () "a" (br ()) "b"))))
  
  (check-fmt 20 "<head> wraps/indents correctly"
             '(html (head (link [[rel "stylesheet"] [href "style.css"]])
                          (meta [[charset "UTF-8"]])
                          (title "onetwothreefour five")))
             '("<!DOCTYPE html>"
               "<html>"
               "  <head>"
               "    <link rel="
               "    \"stylesheet\""
               "    href="
               "    \"style.css\">"
               "    <meta charset="
               "    \"UTF-8\">"
               "    <title>"
               "    onetwothreefour"
               "    five</title>"
               "  </head>"
               "</html>\n"))

  (check-fmt-addbrs 80 "add-breaks does not add breaks in front of meta, link, title"
                    '(html (head (link [[rel "stylesheet"] [href "style.css"]])
                                 (meta [[charset "UTF-8"]])
                                 (title "onetwothreefour five")))
                    '("<!DOCTYPE html>"
                      "<html>"
                      "  <head>"
                      "    <link rel=\"stylesheet\" href=\"style.css\">"
                      "    <meta charset=\"UTF-8\">"
                      "    <title>onetwothreefour five</title>"
                      "  </head>"
                      "</html>\n"))

  (check-fmt 20 "Blocks adjacent to inline elements wrap as expected (narrow; img+figcaption)"
             '(body (figure (img [[src "foo.jpg"]]) (figcaption "Hi")))
             '("<body>"
               "  <figure>"
               "    <img src="
               "    \"foo.jpg\">"
               "    <figcaption>"
               "    Hi</figcaption>"
               "  </figure>"
               "</body>\n"))

  (check-fmt 20 "Blocks adjacent to inline elements wrap as expected (narrow; img+empty figcaption)"
             '(body (figure (img [[src "foo.jpg"]]) (figcaption)))
             '("<body>"
               "  <figure>"
               "    <img src="
               "    \"foo.jpg\">"
               "    <figcaption></figcaption>"
               "  </figure>"
               "</body>\n"))

  (check-fmt 40 "Blocks adjacent to inline elements wrap as expected (wide; img+figcaption)"
             '(body (figure (img [[src "foo.jpg"]]) (figcaption "Hi")))
             '("<body>"
               "  <figure>"
               "    <img src=\"foo.jpg\">"
               "    <figcaption>Hi</figcaption>"
               "  </figure>"
               "</body>\n"))

  (check-fmt 40 "Blocks adjacent to inline elements wrap as expected (wide; img+empty figcaption)"
             '(body (figure (img [[src "foo.jpg"]]) (figcaption)))
             '("<body>"
               "  <figure>"
               "    <img src=\"foo.jpg\">"
               "    <figcaption></figcaption>"
               "  </figure>"
               "</body>\n"))

  (check-fmt 40 "Blocks adjacent to inline elements wrap as expected (wide; inline w/space+empty figcaption)"
             '(body (figure (img [[src "foo.jpg"]]) (b "hi ") (figcaption)))
             '("<body>"
               "  <figure>"
               "    <img src=\"foo.jpg\"><b>hi</b>"
               "    <figcaption></figcaption>"
               "  </figure>"
               "</body>\n"))

  (check-fmt 25 "Tables wrap as expected"
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

  (check-fmt-addbrs 25 "Tables wrap as expected (with added breaks)"
                    '(table (thead (tr (td "Col 1") (td "Col 2") (td "Col 3")))
                            (tbody (tr (td "a") (td "b") (td "c"))))
                    '("<table>"
                      "  <thead>"
                      "    <tr>"
                      "      <td>Col 1</td>"
                      ""
                      "      <td>Col 2</td>"
                      ""
                      "      <td>Col 3</td>"
                      "    </tr>"
                      "  </thead>"
                      ""
                      "  <tbody>"
                      "    <tr>"
                      "      <td>a</td>"
                      ""
                      "      <td>b</td>"
                      ""
                      "      <td>c</td>"
                      "    </tr>"
                      "  </tbody>"
                      "</table>\n"))

  (check-matches-tidy? 25 '(table (thead (tr (td "Col 1") (td "Col 2") (td "Col 3")))
                                  (tbody (tr (td "a") (td "b") (td "c")))))

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
             '("<!DOCTYPE html>"
               "<html>"
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

  (check-fmt 80 "Boolean attributes display properly using HTML5 short syntax"
             '(label (input [[type "checkbox"] [disabled ""]]) "Cheese")
             '("<label><input type=\"checkbox\" disabled>Cheese</label>"))

  (check-matches-tidy? 80 '(label (input [[type "checkbox"] [disabled ""]]) "Cheese"))

  (check-fmt 80 "Whitespace preserved after inline element"
             '(label (input [[type "checkbox"] [disabled ""]]) " Cheese")
             '("<label><input type=\"checkbox\" disabled> Cheese</label>"))

  (check-matches-tidy? 80 '(label (input [[type "checkbox"] [disabled ""]]) " Cheese"))

  (check-fmt 20 "Comments wrap and indent properly inside flows and blocks"
             `(body (main ,(comment "this is\na comment")
                          (article (p "Hi" ,(comment "also another comment")))))
             '("<body>"
               "  <main>"
               "    <!--this is a"
               "    comment-->"
               "    <article>"
               "      <p>Hi<!--also"
               "      another"
               "      comment--></p>"
               "    </article>"
               "  </main>"
               "</body>\n"))

  (check-fmt 45 "Indenting carries through correctly in the presence of comments"
             `(body (main (p "Hi")
                          ,(comment "this is a comment")
                          (p (span "1") ,(comment "another") (em "2"))
                          (blockquote
                           ,(comment "third comment")
                           (p "hello"))))
             '("<body>"
               "  <main>"
               "    <p>Hi</p>"
               "    <!--this is a comment-->"
               "    <p>"
               "    <span>1</span><!--another--><em>2</em></p>"
               "    <blockquote>"
               "      <!--third comment-->"
               "      <p>hello</p>"
               "    </blockquote>"
               "  </main>"
               "</body>\n"))

  (check-fmt 20 "Comments wrap intelligently inside block elements"
             `(main (article (p "Hi " ,(comment "a comment") "hi")))
             '("<main>"
               "  <article>"
               "    <p>Hi <!--a"
               "    comment-->hi</p>"
               "  </article>"
               "</main>\n"))
  
  (check-fmt 20 "Comment wrapping does not introduce whitespace between otherwise-adjacent strings"
             `(main (article (p "Hi" ,(comment "a comment") "hi")))
             '("<main>"
               "  <article>"
               "    <p>Hi<!--a"
               "    comment-->hi</p>"
               "  </article>"
               "</main>\n"))

  (check-fmt 20 "CDATA printed unmodified"
             `(main (article (p "Hi " ,(cdata #f #f "<![CDATA[Oṃ maṇi padme hūm̐]]>") "hi")))
             '("<main>"
               "  <article>"
               "    <p>Hi"
               "    <![CDATA[Oṃ maṇi padme hūm̐]]>"
               "    hi</p>"
               "  </article>"
               "</main>\n"))

  (check-fmt 20 "CDATA wrapping does not introduce whitespace between otherwise-adjacent strings"
             `(main (article (p "Hi" ,(cdata #f #f "<![CDATA[Oṃ maṇi padme hūm̐]]>") "hi")))
             '("<main>"
               "  <article>"
               "    <p>"
               "    Hi<![CDATA[Oṃ maṇi padme hūm̐]]>hi</p>"
               "  </article>"
               "</main>\n"))

  (check-fmt 20 "Definition lists wrap properly"
             '(body (article (dl (dt "Term") (dd (p "Def")))))
             '("<body>"
               "  <article>"
               "    <dl>"
               "      <dt>"
               "        Term"
               "      </dt>"
               "      <dd>"
               "        <p>Def</p>"
               "      </dd>"
               "    </dl>"
               "  </article>"
               "</body>\n"))

  (check-fmt 20 "Tolerate busted/bad HTML structure (flow inside non-flow)"
             '(body (p (em "Hello " (div "World")) "woah"))
             '("<body>"
               "  <p><em>Hello"
               "    <div>"
               "      World"
               "    </div>"
               "  </em>woah</p>"
               "</body>\n"))
  
  ; self-closing tags display properly
  (for ([tag (in-list '(area base basefont br col frame hr img input isindex link meta param))])
    (check-fmt 20
               (format "~a displays correctly as self-closing tag" tag)
               `(,tag)
               (list (format "<~a>~a"
                             tag
                             (if (or (block? tag) (flow? tag) (eq? tag 'br)) (sys-newline) "")))))

  (check-fmt 40 "Aside elements wrap properly"
             '(body (article (p "One") (aside [[class "me"]] "Two") (aside [[class "me"]] (p "three"))))
             '("<body>"
               "  <article>"
               "    <p>One</p>"
               "    <aside class=\"me\">"
               "      Two"
               "    </aside>"
               "    <aside class=\"me\">"
               "      <p>three</p>"
               "    </aside>"
               "  </article>"
               "</body>\n"))

  (check-matches-tidy? 40 '(body (article (p "One") (aside [[class "me"]] "Two") (aside [[class "me"]] (p "three")))))

  (check-fmt 40 "Indents for <br> after flow and block close"
             '(body (table (tr (td "Hi"))) (br) (br) (p "Paragraph") (br))
             '("<body>"
               "  <table>"
               "    <tr>"
               "      <td>Hi</td>"
               "    </tr>"
               "  </table>"
               "  <br>"
               "  <br>"
               "  <p>Paragraph</p>"
               "  <br>"
               "</body>\n"))

  (check-fmt 40 "Custom elements wrapped as flow tags"
             '(body (article (my-element (i "Hello") " World")))
             '("<body>"
               "  <article>"
               "    <my-element>"
               "      <i>Hello</i> World"
               "    </my-element>"
               "  </article>"
               "</body>\n"))

  (check-fmt 40 "Empty custom elements"
             '(body (article (my-element)))
             '("<body>"
               "  <article>"
               "    <my-element>"
               "    </my-element>"
               "  </article>"
               "</body>\n"))

  ;;================================================
  ;; Regression tests for issues found in the original printer

  (check-fmt 16 "Lines may fill the wrap width exactly (first word glued to opening tag)"
             '(p "abcdefghijklm nop")
             '("<p>abcdefghijklm"
               "nop</p>\n"))

  (check-matches-tidy? 16 '(p "abcdefghijklm nop"))

  (check-fmt 16 "Lines may fill the wrap width exactly (word preceded by a space)"
             '(p "abcdef ghijkl m")
             '("<p>abcdef ghijkl"
               "m</p>\n"))

  (check-matches-tidy? 16 '(p "abcdef ghijkl m"))

  (check-fmt 12 "Whitespace-only strings between inline elements allow wrapping"
             '(p (em "aaa") " " (em "bbb") " " (em "ccc"))
             '("<p>"
               "<em>aaa</em>"
               "<em>bbb</em>"
               "<em>ccc</em></p>\n"))

  (check-matches-tidy? 30 '(p (em "aaa") " " (em "bbb") " " (em "ccc")))

  (check-fmt 12 "Whitespace-only strings between plain strings allow wrapping"
             '(p "aaaa" " " "bbbb" " " "cccc")
             '("<p>aaaa bbbb"
               "cccc</p>\n"))

  (check-matches-tidy? 12 '(p "aaaa" " " "bbbb" " " "cccc"))

  (check-fmt 25 "Newline strings between inline elements allow wrapping"
             '(p (a ((href "x")) "one") "\n" (a ((href "y")) "two") "\n" (a ((href "z")) "three"))
             '("<p><a href=\"x\">one</a>"
               "<a href=\"y\">two</a>"
               "<a href=\"z\">three</a></p>\n"))

  (check-matches-tidy? 30 '(p (a ((href "x")) "one") "\n" (a ((href "y")) "two") "\n" (a ((href "z")) "three")))

  (check-fmt 12 "Inline closing tag stays glued to its content when trailing whitespace is hoisted"
             '(p (em "aaa ") (em "bbb ") (em "ccc"))
             '("<p>"
               "<em>aaa</em>"
               "<em>bbb</em>"
               "<em>ccc</em></p>\n"))

  (check-matches-tidy? 30 '(p (em "aaa ") (em "bbb ") (em "ccc")))

  (check-fmt 40 "Flow element after text inside a flow parent is indented as a sibling of the text"
             '(ul (li "Item" (ul (li "sub"))))
             '("<ul>"
               "  <li>"
               "    Item"
               "    <ul>"
               "      <li>"
               "        sub"
               "      </li>"
               "    </ul>"
               "  </li>"
               "</ul>\n"))

  (check-fmt 40 "Text following a closed block inside a flow is indented"
             '(body (div (p "para") "bye" (p "again")))
             '("<body>"
               "  <div>"
               "    <p>para</p>"
               "    bye"
               "    <p>again</p>"
               "  </div>"
               "</body>\n"))

  (check-fmt 20 "No trailing spaces when wrapping between attributes"
             '(div (a ((href "x") (class "yyyyy") (id "zzzzz")) "t"))
             '("<div>"
               "  <a href=\"x\" class="
               "  \"yyyyy\" id="
               "  \"zzzzz\">t</a>"
               "</div>\n"))

  (check-matches-tidy? 20 '(div (a ((href "x") (class "yyyyy") (id "zzzzz")) "t")))

  (check-fmt 12 "Indent deeper than the wrap width never produces blank lines"
             '(body (div (div (div (div (div (p "word word word")))))))
             '("<body>"
               "  <div>"
               "    <div>"
               "      <div>"
               "        <div>"
               "          <div>"
               "            <p>"
               "            word"
               "            word"
               "            word</p>"
               "          </div>"
               "        </div>"
               "      </div>"
               "    </div>"
               "  </div>"
               "</body>\n"))

  (check-matches-tidy? 12 '(body (div (div (div (div (div (p "word word word"))))))))

  (check-fmt 20 "Whitespace containing newlines after a comment is collapsed, not printed verbatim"
             `(p "Hi" ,(comment "x") " \n y")
             '("<p>Hi<!--x--> y</p>\n"))

  (check-matches-tidy? 20 `(p "Hi" ,(comment "x") " \n y"))

  (check-fmt 40 "Closing tag of <pre> is at column 1 when content ends with a newline"
             '(div (pre "line one\nline two \n"))
             '("<div>"
               "  <pre>line one"
               "line two "
               "</pre>"
               "</div>\n"))

  (check-matches-tidy? 40 '(div (pre "line one\nline two \n")))

  (check-fmt 40 "Closing tag of <pre> is glued when content does not end with a newline"
             '(div (pre "line one\nline two"))
             '("<div>"
               "  <pre>line one"
               "line two</pre>"
               "</div>\n"))

  (check-matches-tidy? 40 '(div (pre "line one\nline two")))

  ;;================================================
  ;; Property-based check: random documents at random widths must render like their input, never end
  ;; a line in whitespace, and never break a line that could have been broken earlier. Deterministic
  ;; for the given seed; see tests/fuzz.rkt for the environment variables that make it run harder.

  (check-fuzz 300 20260906)
  )
