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

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Public parameters
;;

(define minimum-tidy-version "5.8.0")
(define tidy-path (make-parameter #f (λ (v) (_unset-tidy-version!) v)))
(define tidy-options (make-parameter "-quiet -indent --wrap-attributes no --tidy-mark no"))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; HTML Tidy commands and version checks
;;

;; Return the first one of
;;  • tidy-path (parameter)
;;  • cached path from previous result
;;  • HTML_TIDY_PATH environment variable
;;  • "tidy" executable on system PATH
;;
;; …that points to an existing file for which the user has execute permissions.
;;
(define _tidy-path/param-or-cached
  (let ([tp #f])
    (define (maybe-executable p)
      (and p
           (file-exists? p)
           (member 'execute (file-or-directory-permissions p))
           p))
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

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; HTML Tidy interface
;;

;; X-expression → HTML string (output of HTML tidy)
;;
;; The output inside the first matching #:extract-tag is returned.
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

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; HTML Tidy private utilities
;;

;; Clothe an x-expression in a <head> or <body> tag if it doesn't already
;; start with one of those tags.
(define (xpr x)
  (or (and (member (car x) '(head body)) x)
      `(body (main (article ,x)))))

;; Tidy likes to operate on complete HTML5 documents.
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