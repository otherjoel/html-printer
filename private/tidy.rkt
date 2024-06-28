#lang racket/base

(require  "semver.rkt"
          racket/match
          racket/port
          racket/string
          racket/system
          xml)

(provide tidy-path
         tidy-options
         get-tidy-version
         tidy-version-sufficient?
         tidy
         xpr)

(define minimum-tidy-version "5.8.0")
(define tidy-path (make-parameter #f (λ (v) (_unset-tidy-version!) v)))
(define tidy-options (make-parameter "-quiet -indent --wrap-attributes no --tidy-mark no"))

(define (maybe-executable p)
  (and p
       (file-exists? p)
       (member 'execute (file-or-directory-permissions p))
       p))

; If (tidy-path) is non-false and points to an existing executable
(define _tidy-path/param-or-cached
  (let ([tp #f])
    (lambda ()
      (cond
        [(maybe-executable (tidy-path))]
        [tp]
        [else
         (set! tp (or (maybe-executable (getenv "HTML_TIDY_PATH"))
                      (find-executable-path "tidy")))
         tp]))))

(define-values (get-tidy-version
                tidy-version-sufficient?
                _unset-tidy-version!)
  (let ([tidy-version #f]
        [version-sufficient-flag #f])
    (define (_getv)
      (cond
        [tidy-version]
        [else
         (define v-str (try-extract-version (try-tidy-version-cmd)))
         (set! tidy-version (or v-str "0.0.0"))
         tidy-version]))
    (define (_vs?)
      (case version-sufficient-flag
        [(#f)
         (define cmp (version>=? (_getv) minimum-tidy-version))
         (set! version-sufficient-flag (if cmp 'yes 'no))
         cmp]
        [(yes) #t]
        [(no) #f]))
    (define (_unset!)
      (set! tidy-version #f)
      (set! version-sufficient-flag #f))
    (values _getv _vs? _unset!)))

(define (try-tidy-version-cmd)
  (match (_tidy-path/param-or-cached)
    [(? string? tp)
     (with-output-to-string (lambda () (system (format "~a --version" tp))))]
    [_ #f]))

(define (tidy/raw [xp '(p "a")] [wrap-col 100])
  (define tp (_tidy-path/param-or-cached))
  (define opts (format "~a --wrap ~a" (tidy-options) wrap-col))
  (parameterize ([current-input-port (open-input-string (htmlify xp))])
    (with-output-to-string
      (lambda ()
        (system (format "~a ~a" tp opts))))))

(define (tidy xp
              #:extract-tag [tag (or (and (eq? 'head (car xp)) 'head) 'body)]
              #:wrap [wrap-col 100])
  (cond
    [(tidy-version-sufficient?)
     (define tp (_tidy-path/param-or-cached))
     (define opts
       (format "~a --wrap ~a" (tidy-options) wrap-col))
     (define result
       (parameterize ([current-input-port (open-input-string (htmlify xp))])
         (with-output-to-string
           (lambda ()
             (system (format "~a ~a" tp opts))))))
     (car (regexp-match (regexp (format "(<~a>.+</~a>)" tag tag)) result))]
    [else #f]))

(define (xpr x)
  (or (and (member (car x) '(head body)) x)
      `(body (main (article ,x)))))

(define (htmlify x)
  (case (car x)
    [(head)
     (format "<!DOCTYPE html><html lang=\"en\">~a<body></body></html>"
             (xexpr->string x))]
    [(body)
     (format "<!DOCTYPE html><html lang=\"en\"><head><title>Test</title></head>~a</html>"
             (xexpr->string x))]
    [else
     (format "<!DOCTYPE html><html lang=\"en\"><head><title>Test</title></head>~a</html>"
             (xexpr->string (xpr x)))]))