`html-printer`
==============

A Racket library for converting X-expressions to strings of HTML with content-aware line wrapping
and indentation. Comments and PRs welcome. 

**Documentation is at <https://joeldueck.com/what-about/html-printer>**

This package requires Racket 8.13 or higher.

To install:

    > raco pkg install html-printer

## Lingering questions…

- How much logging/debugging instrumentation should be left in? (Probably none except for errors,
  but see next question)

- What should happen when an X-expression's structure is not valid HTML, such as a `<div>` inside a
  `<p>`? Currently it just logs an error, but maybe it should throw an exception?
