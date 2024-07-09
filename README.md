html-writer
===========

A Racket library for converting X-expressions to strings of HTML with content-aware line wrapping
and indentation.

Expected benefits of this library over [`xexpr->string`][1], [`xexpr->html`][2] and
[`display-xml/content`][3]:

* Wraps and indents based on the tag type according to HTML5 standard (block/flow/inline) rather
  than based on scanning a tag's contents
* Allows you to set the column width of the output
* Never inserts a line break where doing so would introduce whitespace in the HTML output
* Attribute values are never broken across lines
* Does not modify content of `<script>` and `<style>` tags
* Unicode-aware: wraps based on grapheme count vs. character count (depends on
  [`unicode-breaks`][ub])
* Outputs boolean attributes the HTML5 way (`<option selected>` rather than `<option
  selected="selected">`)
* Outputs self-closing tags the HTML5 way (`<meta charset="UTF-8">`, note `>` to close rather than
  `/>`)

[1]: https://docs.racket-lang.org/xml/index.html#%28def._%28%28lib._xml%2Fmain..rkt%29._xexpr-~3estring%29%29
[2]: https://docs.racket-lang.org/txexpr/index.html#%28def._%28%28lib._txexpr%2Fmain..rkt%29._xexpr-~3ehtml%29%29
[3]: https://docs.racket-lang.org/xml/index.html#%28def._%28%28lib._xml%2Fmain..rkt%29._display-xml%2Fcontent%29%29
[ub]: https://docs.racket-lang.org/unicode-breaks/index.html

Still in early stages, comments and PRs welcome. 

See the tests at the end of main.rkt to get an idea of what it can do so far.  The tests that are
there are passing at this point. Lots to do though:

- [x] ~~Add many more tests~~
- [x] ~~Add lookahead (or deferred output) for smarter wrapping~~
- [x] ~~Defer whitespace to avoid trailing spaces on lines (update tests first)~~
- [x] ~~Thoroughly compare output against HTML Tidy~~
- Add option to self-close tags the XHTML way (`/>`)
- Split tests into their own files
- Documentation

## Questions to resolve

- How much logging/debugging instrumentation should be left in? (Probably none except for errors,
  but see next question)

- What should happen when an X-expression's structure is not valid HTML, such as a `<div>` inside a
  `<p>`? Currently it just logs an error, but maybe it should throw an exception?

- Support CDATA/PCDATA? (Probably not?) If not, then the things we are converting maybe aren’t
  technically X-expressions, what exactly are they and how should they be validated?
