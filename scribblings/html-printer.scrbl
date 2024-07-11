#lang scribble/manual

@(require scribble/examples)

@require[html-printer/private/tidy
         @for-label[html-printer
                    html-printer/debug
                    racket/base
                    txexpr
                    xml]]

@(define examps (make-base-eval #:lang 'racket/base))
@(examps '(require html-printer html-printer/debug txexpr xml))

@title{HTML5 Printer}
@author{Joel Dueck}

@defmodule[html-printer]

If you use Racket to generate web pages, you should use this package to make your HTML both readable
and correct. Why go to the trouble of designing clean, semantic markup if you’re just going to slap
it all on one line that scrolls horizontally forever?

This package provides a single function — @racket[xexpr->html5] — for converting X-expressions to
strings of HTML. Unlike other functions which can be used for this purpose, this one is focused on a
tidy presentation of HTML5 content specifically. It indents and wraps lines, allowing you to set the
width of the output, while ensuring line breaks aren’t placed where they would create syntactic
difference from the input. It favors HTML5 syntax over XML syntax in some cases.

This package is also Unicode-aware, measuring line length in graphemes rather than characters. So,
for example the emoji 🧝‍♂️ — which actually consists of four Unicode “characters” — is counted
    as having length 1 rather than 4.

If you encounter a bug, please open an issue on
@hyperlink["https://github.com/otherjoel/html-printer"]{the GitHub repo}.

Requires Racket 8.13 or later due to internal use of @racketmodname[racket/mutable-treelist].

@defproc[(xexpr->html5 [xpr xexpr?] [#:wrap wrap-col exact-positive-integer? 100]) string?]{

Converts @racket[_xpr] to a string of HTML, nicely wrapped and indented, ready for consumption.
Leave @racket[_wrap-col] at its default of 100 columns, or shrink it down hard to test the
line-wrapping algorithm.

 @examples[#:eval examps
           (display
            (xexpr->html5 #:wrap 25
                          '(body
                            (article
                             (h1 "My Title")
                             (p "Welcome to the blog!")))))]

That’s all there is to it, really. But if you want more info, check out the @secref{deets}.

}

@section[#:tag "deets"]{Crunchy details}

This package includes @hyperlink["https://github.com/otherjoel/html-printer/blob/main/test.rkt"]{an
extensive set of unit tests}. In addition to preventing regressions, these nicely illustrate the
printer’s expected behavior in a variety of edge cases.

@subsection{HTML particulars}

@bold{Escaping special characters:} Any @litchar{<}, @litchar{>} and @litchar{&} characters in
string elements are escaped, and any symbols or integers in element position are converted to
character entities:

@examples[#:eval examps #:label #f
          (display (xexpr->html5 '(p "Entities: " nbsp 65)))
          (display (xexpr->html5 '(p "Escaping < > &")))]

In attribute values, the @litchar{"} character is escaped in addition to @litchar{<}, @litchar{>}
and @litchar{&} characters:

@examples[#:eval examps #:label #f
          (display (xexpr->html5 '(p [[data-desc "Escaping \" < > &"]] "Foo")))]

The contents of @racketoutput{<style>} and @racketoutput{<script>} tags are never escaped or
wrapped; the contents of @racketoutput{<pre>} tags @emph{are} escaped, but never wrapped.

@examples[#:eval examps #:label #f
          (display
           (xexpr->html5 '(body (style "/* No escaping! & < > \" */")
                                (script "/* No escaping! & < > \" */")
                                (pre "Escaping! & < > \""))))]

The printer can handle XML @racket[comment] and @racket[cdata] elements. Comments are line-wrapped
and indented like everything else. CDATA content is never modified or escaped.

@examples[#:eval examps #:label #f
          (define com (comment "Behold, a hidden comment & < >"))
          (define cd (cdata #f #f "<![CDATA[Also some of this & < > ]]>"))
          (display
           (xexpr->html5 #:wrap 20 `(body (article (h1 "Title" ,com) (p ,cd " foo")))))]

@bold{Differences from XML/XHTML:} Attributes which
@hyperlink["https://html.spec.whatwg.org/multipage/indices.html#attributes-3"]{the HTML5 spec
identifies as boolean attributes} are printed using the HTML5 “short” syntax. So, for example when
@code{'(disabled "true")} is supplied as an attribute, it is printed as @racketoutput{disabled}
rather than @racketoutput{disabled=""} or @racketoutput{disabled="disabled"}.

 @examples[#:eval examps #:label #f
           (display
            (xexpr->html5 '(label (input [[type "checkbox"] [disabled ""]]) " Cheese")))]

HTML elements which cannot have content
(@hyperlink["https://developer.mozilla.org/en-US/docs/Glossary/Void_element"]{void elements}) are
ended with @litchar{>} (rather than with @litchar{/>} as in XML):

 @examples[#:eval examps #:label #f
           (display (xexpr->html5 '(div (img [[src "cat.webp"]]))))
           (display (xexpr->html5 '(head (meta [[charset "UTF-8"]]))))]

@subsection{Comparing with included Racket functions}

Racket already includes a few functions for printing X-expressions in string form. These work just
fine generic XML markup; but for use as HTML content, the markup they generate can be incorrect or
suboptimal.

In particular, all three of these functions will escape @litchar{<}, @litchar{>} and @litchar{&}
characters inside @racketoutput{<script>} and @racketoutput{<style>} tags, which is likely to
introduce JavaScript and CSS errors.

The @racket[xexpr->string] function is the simplest. It does not offer line wrapping or indentation:

 @examples[#:eval examps #:label #f
           (xexpr->string '(body (main (script "3 > 2"))))]

The @racket[display-xml/content] function (in combination with @racket[xexpr->xml]) offers options
for indentation, but the docs warn that in HTML applications additional whitespace may be
introduced. It does not support wrapping lines beyond a maximum width.

@examples[#:eval examps #:label #f
          (code:comment @#,elem{Will render incorrectly as "Hello World"})
          (code:comment @#,elem{due to the added line break})
          (display-xml/content
           (xexpr->xml '(body (article (p (b "Hello") (i "World")))))
           #:indentation 'scan)

          (code:comment @#,elem{HTML5 printer will leave lines long})
          (code:comment @#,elem{rather than add significant whitespace})
          (display
           (xexpr->html5 #:wrap 20
                         '(body (article (p (b "Hello") (i "World"))))))
          ]

The @racket[write-xexpr] function has the same shortcomings as those already mentioned, and comes
with its own very odd optional line wrapping scheme: adding a line break before the closing
@litchar{>} of every opening tag.

@examples[#:eval examps #:label #f
          (write-xexpr '(body (article (p (b "Hello") (i "World")))))
          (write-xexpr '(body (article (p (b "Hello") (i "World"))))
                        #:insert-newlines? #t)]

@subsection{Comparing with @tt{xexpr->html}}

The @racketmodname[txexpr] package includes @racket[xexpr->html], which correctly avoids escaping
special characters inside @racketoutput{<script>} and @racketoutput{<style>} tags. Its HTML output
will always be correct and faithful to the input, but since it performs no wrapping or indentation,
the output can be difficult to read without additional processing.

@examples[#:eval examps #:label #f
          (define xp '(html
                       (head
                        (style "/* < > & */"))
                       (body
                        (section (h1 "Beginning"))
                        (section (h1 "End")))))
          (display (xexpr->html xp))
          (display (xexpr->html5 xp #:wrap 20))]

@subsection{Comparing with HTML Tidy}

The @hyperlink["https://www.html-tidy.org"]{HTML Tidy} console application has been the best
available tool for linting, correcting and formatting HTML markup since its creation in 1994. Its
original purpose was to correct errors in HTML files written by hand in text editors.

Tidy is a much more comprehensive tool than this one and much more configurable. It always produces
correctly line-wrapped and indented HTML, though this is only part of its functionality. In terms of
formatting functionality specifically, the only significant difference betweeen Tidy and this
package is that Tidy still counts line width by characters rather than graphemes, so it may wrap
lines earlier than necessary when they contain emoji or other multi-byte graphemes.

@margin-note{Note that MacOS ships with an old version of HTML Tidy, but it's too old for use with
modern HTML.}

This package includes unit tests which compare its output against that of HTML Tidy in some cases.
When tests are run (including at the time of package installation), it will search for a version of
Tidy version @minimum-tidy-version or newer, first in the @envvar{HTML_TIDY_PATH} environment
variable, then in the current @envvar{PATH}; if found, these unit tests will be run normally.
Otherwise, the tests will pass without any comparison actually being made.

@subsection{Probing and prodding}

@defmodule[html-printer/debug]

I lied at the beginning of these docs when I said this package only provides a single function. Here
are a couple more, though they will only be interesting to people who really want to kick the tires.

@deftogether[(
             
@defproc[(proof [x xexpr?] [#:wrap wrap 20]) void?]
 
@defproc[(debug [x xexpr?] [#:wrap wrap 20]) void?]

)]{

Used for a close visual inspection of line wrapping and indentation, @racket[proof] displays the
result of @racket[(xexpr->html5 x #:wrap wrap)] but with a column rule at the top and whitespace
characters made visible:

@examples[#:eval examps #:label #f
          (proof '(p "Chaucer, Rabelais and " (em "Balzac!")))]

The @racket[debug] function does the same thing but spits out an ungodly amount of gross logging on
@racket[(current-error-port)], for use in debugging the printing algorithm. This function may
become a no-op or disappear altogether in the future.

@examples[#:eval examps #:label #f
          (debug '(p "Chaucer, Rabelais and " (em "Balzac!")))]

}

