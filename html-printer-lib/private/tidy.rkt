#lang racket/base

(require  "semver.rkt"
          racket/match
          racket/port
          racket/string
          racket/system
          xml)

;; Provides an interface to a stable release of HTML Tidy >= 5.8.0, if available on the local system.

;; This module is primarily written for use with the html-writer testing harness.
;; Use elsewhere at your own risk.

(provide tidy-path
         tidy-options
         get-tidy-version
         minimum-tidy-version
         tidy-version-sufficient?
         tidy
         xpr)

(module+ test)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Public parameters
;;

(define minimum-tidy-version "5.8.0")
(define tidy-path (make-parameter #f))
(define tidy-options (make-parameter '("-quiet" "-indent" "--wrap-attributes" "no" "--tidy-mark" "no")))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; HTML Tidy commands and version checks
;;

;; Return the first one of
;;  • tidy-path (parameter)
;;  • HTML_TIDY_PATH environment variable
;;  • "tidy" executable on system PATH
;;
;; …that points to an existing file for which the user has execute permissions, or #f if none does.
;; The environment/PATH search is done at most once per process.
;;
(define resolve-tidy-path
  (let ([searched? #f]
        [found #f])
    (define (executable p)
      (and (or (path? p) (non-empty-string? p))
           (file-exists? p)
           (member 'execute (file-or-directory-permissions p))
           p))
    (lambda ()
      (or (executable (tidy-path))
          (begin
            (unless searched?
              (set! searched? #t)
              (set! found (or (executable (getenv "HTML_TIDY_PATH"))
                              (find-executable-path "tidy"))))
            found)))))

;; Version string of the resolved Tidy executable, or "0.0.0" if none was found.
;; The result is cached together with the path it came from, so a change to the tidy-path
;; parameter (including entering or leaving a parameterize) triggers a fresh probe.
(define get-tidy-version
  (let ([cached-path #f]
        [cached-version #f])
    (lambda ()
      (define p (resolve-tidy-path))
      (unless (and cached-version (equal? p cached-path))
        (set! cached-path p)
        (set! cached-version (or (and p (try-extract-version (run-tidy p "--version")))
                                 "0.0.0")))
      cached-version)))

;; Stable release (even minor version) of Tidy >= minimum-tidy-version available?
(define (tidy-version-sufficient?)
  (define v (get-tidy-version))
  (and (version>=? v minimum-tidy-version)
       (even? (minor-version v))))

;; Run Tidy with the given arguments; return its standard output as a string.
;; Standard input comes from current-input-port; standard error is discarded.
(define (run-tidy p . args)
  (parameterize ([current-error-port (open-output-nowhere)])
    (with-output-to-string (lambda () (apply system* p args)))))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; HTML Tidy interface
;;

;; X-expression → HTML string (output of HTML tidy)
;;                or #f if a stable release of Tidy >= 5.8.0 is not available,
;;                or if Tidy's output does not contain the #:extract-tag element.
;;
;; The output inside the first matching #:extract-tag is returned.
(define (tidy xp
              #:extract-tag [tag (or (and (eq? 'head (car xp)) 'head) 'body)]
              #:wrap [wrap-col 100])
  (cond
    [(tidy-version-sufficient?)
     (define result
       (parameterize ([current-input-port (open-input-string (htmlify xp))])
         (apply run-tidy (resolve-tidy-path)
                (append (tidy-options) (list "--wrap" (number->string wrap-col))))))
     (match (regexp-match (regexp (format "(<~a>.+</~a>)" tag tag)) result)
       [(list m _) m]
       [_ #f])]
    [else #f]))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; HTML Tidy private utilities
;;

;; Clothe an x-expression in a <body> tag if it looks like it needs one
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