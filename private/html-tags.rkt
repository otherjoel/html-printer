#lang racket/base

(require racket/symbol)

(provide (all-defined-out))

(module+ test)

(define default-block-tags
  '(address canvas fieldset figcaption h1 h2 h3 h4 h5 h6 hr link noscript output p title tr td meta
            video))

(define default-flow-tags
  '(article aside blockquote body div figure footer form html head header hgroup li main nav ol
            section table tbody thead tfoot tr ul dd dl dt))

(define default-selfclose-tags
  '(area base basefont br col embed frame hr img input isindex link meta param source track wbr))

;; https://html.spec.whatwg.org/multipage/indices.html#attributes-3
(define html5-boolean-attrs
  '(allowfullscreen async autofocus autoplay checked controls default defer disabled formnovalidate
                    inert ismap itemscope loop multiple muted nomodule novalidate open playsinline
                    readonly required reversed selected))

(define (symbol-downcase sym)
  (string->symbol (string-downcase (symbol->immutable-string sym))))

;;================================================
;; Custom element names
;; https://html.spec.whatwg.org/multipage/custom-elements.html#custom-elements-core-concepts

(define PCENChar
  (string-append "[-\\.0-9_a-z"
                 "\u00B7"
                 "\u00C0-\u00D6"
                 "\u00D8-\u00F6"
                 "\u00F8-\u037D"
                 "\u037F-\u1FFF"
                 "\u200C-\u200D"
                 "\u203F-\u2040"
                 "\u2070-\u218F"
                 "\u2C00-\u2FEF"
                 "\u3001-\uD7FF"
                 "\uF900-\uFDCF"
                 "\uFDF0-\uFFFD"
                 "\U00010000-\U000EFFFF]"))

(define cust-elem-name-rx
  (regexp (string-append "^[a-z]"
                         PCENChar "*"
                         "-"
                         PCENChar "*$")))

(define (valid-custom-element-name? v)
  (regexp-match? cust-elem-name-rx
                 (if (symbol? v)
                     (symbol->immutable-string v)
                     v)))

;;================================================
;; Tag predicates

;; Treat custom elements as "flow" for indent/wrapping purposes
;; Note this test works without checking other lists because custom elements are required to include
;; a hyphen, and no HTML tags have hyphens.
(define (flow? tag) (or (memq (symbol-downcase tag) default-flow-tags)
                        (valid-custom-element-name? tag)))

(define (block? tag)             (memq (symbol-downcase tag) default-block-tags))
(define (preserve-contents? tag) (memq (symbol-downcase tag) '(script style pre)))
(define (self-closing? tag)      (memq (symbol-downcase tag) default-selfclose-tags))
(define (boolean-attr? a)        (memq (symbol-downcase a) html5-boolean-attrs))
(define (br? tag)                (eq? (symbol-downcase tag) 'br))

