#lang racket/base

(require racket/symbol)

(provide (all-defined-out))

(module+ test)

(define default-block-tags
  '(address article aside canvas fieldset figcaption
            h1 h2 h3 h4 h5 h6 hgroup hr link noscript output p section title tr td meta
            video))

(define default-flow-tags
  '(article blockquote body div figure footer form html head header li main nav ol section table tbody
            thead tfoot tr ul dd dl dt))

(define default-selfclose-tags
  '(area base basefont br col embed frame hr img input isindex link meta param source track wbr))

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

