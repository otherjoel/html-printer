#lang racket/base

(require  (prefix-in base: xml)
          racket/port
          racket/string
          racket/system)

(provide (all-defined-out))

(define (xpr x)
  (or (and (member (car x) '(head body)) x)
      `(body (main (article ,x)))))

(define (htmlify x)
  (case (car x)
    [(head)
     (format "<!DOCTYPE html><html lang=\"en\">~a<body></body></html>"
             (base:xexpr->string x))]
    [(body)
     (format "<!DOCTYPE html><html lang=\"en\"><head><title>Test</title></head>~a</html>"
             (base:xexpr->string x))]
    [else
     (format "<!DOCTYPE html><html lang=\"en\"><head><title>Test</title></head>~a</html>"
             (base:xexpr->string (xpr x)))]))

(define tidy-path (make-parameter "/opt/homebrew/bin/tidy"))
(define tidy-options (make-parameter "-quiet -indent --wrap-attributes no --tidy-mark no"))

(define (tidy/raw xp [wrap-col 100])
  (define opts (string-split (format "~a --wrap ~a" (tidy-options) wrap-col)))
  (with-output-to-string
    (lambda ()
      (parameterize ([current-input-port (open-input-string (htmlify xp))])
        (apply system* (tidy-path) opts)))))

(define (tidy xpr #:extract-tag [tag (or (and (eq? 'head (car xpr)) 'head) 'body)] #:wrap [wrap-col 100])
  (car (regexp-match (regexp (format "(<~a>.+</~a>)" tag tag)) (tidy/raw xpr wrap-col))))