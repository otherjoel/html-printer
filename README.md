html-writer
===========

A Racket library for converting X-expressions to strings of HTML with content-aware line wrapping
and indentation.

Goals:

* Wraps and indents based on the tag type according to HTML5 standard (block/flow/inline) rather
  than based on scanning a tag's contents
* Allows you to set the column width of the output
* Never inserts a line break where doing so would introduce whitespace in the HTML output
* Attribute values are never broken across lines
* Unicode-aware: wraps based on grapheme count vs. character count (depends on
  [`unicode-breaks`][ub])
* Outputs boolean attributes the HTML5 way (`<option selected>` rather than `<option
  selected="selected">`)
* Outputs self-closing tags the HTML5 way (`<meta charset="UTF-8">`, note `>` to close rather than
  `/>`)

[ub]: https://docs.racket-lang.org/unicode-breaks/index.html

Still in early stages, comments and PRs welcome. 

See the tests at the end of main.rkt to get an idea of what it can do so far.  The tests that are
there are passing at this point. Lots to do though!
