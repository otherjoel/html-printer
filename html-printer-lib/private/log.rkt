#lang racket/base

(require (for-syntax racket/base)
         racket/list
         racket/logging
         racket/string)

(provide html-printer-logger
         logging-enabled?
         logging-phases
         log-expr            ; Debug info at the xexpr-walking level
         log-tokens          ; The token stream handed from the walker to the printer
         log-printer         ; Debug info at the low-level printer level
         logging-to-stderr)

(module+ test)

;; All logging is at the 'debug level, on one topic per phase of the printing process:
;;
;;   html-printer/expr      the walk over the X-expression (main.rkt)
;;   html-printer/tokens    the token stream produced by the walk
;;   html-printer/printer   the wrapping printer consuming the tokens (printer.rkt)
;;
;; Every logging form is guarded by (logging-enabled?), so that when logging is off the cost is
;; one parameter lookup and no formatting.

(define-logger html-printer)
(define logging-enabled? (make-parameter #f))
(define logging-phases '(expr tokens printer))

(define (phase->topic phase)
  (case phase
    [(expr) 'html-printer/expr]
    [(tokens) 'html-printer/tokens]
    [(printer) 'html-printer/printer]
    [else (raise-argument-error 'logging-to-stderr "one of 'expr, 'tokens, 'printer" phase)]))

;; Send a line to the logger. The topic is not prefixed to the message: each phase writes its own
;; short tag instead.
(define (log! phase str)
  (log-message html-printer-logger 'debug (phase->topic phase) str #f #f))

;; Run proc with the selected phases of logging written to (current-error-port)
(define (logging-to-stderr proc #:show [phases logging-phases])
  (define spec (append* (for/list ([p (in-list phases)]) (list 'debug (phase->topic p)))))
  (apply with-logging-to-port (current-error-port) proc spec #:logger html-printer-logger))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Walker phase
;;
;; (log-expr DEPTH OP [MSG] VAR ...)
;;
;; DEPTH is the nesting depth of the element in the X-expression, used for indentation.
;; OP is a bare identifier naming the event, e.g. flow or /flow.
;; MSG, if present, must be a string literal.
;; Each VAR must be an identifier; it is shown as name=value.
;;
;; EXPR   block p  parent=flow prev-block?=#f

(define-for-syntax (split-message args)
  ;; → (values message-syntax-or-#f vars-syntax-list)
  (syntax-case args ()
    [(msg var ...)
     (string? (syntax-e #'msg))
     (values #'msg (syntax->list #'(var ...)))]
    [(var ...)
     (values #f (syntax->list #'(var ...)))]))

(define-syntax (log-expr stx)
  (syntax-case stx ()
    [(_ DEPTH OP ARG ...)
     (let-values ([(msg vars) (split-message #'(ARG ...))])
       (with-syntax ([MSG (or msg #'#f)]
                     [(VAR ...) vars])
         #'(and (logging-enabled?)
                (log! 'expr ($expr-line DEPTH 'OP MSG '(VAR ...) (list VAR ...))))))]))

(define ($expr-line depth op msg names vals)
  (string-append "EXPR "
                 (make-string (* 2 depth) #\space)
                 (symbol->string op)
                 (if msg (string-append " " msg) "")
                 (if (null? names) "" "  ")
                 ($vars names vals)))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Token stream
;;
;; (log-tokens strs) where strs is a list of tokens already converted to strings.
;; One log line per output line: the stream is broken after every newline and blank token.
;;
;; TOKENS newline "<div>" indent+ newline

(define (log-tokens strs)
  (define (line! toks)
    (unless (null? toks)
      (log! 'tokens (string-append "TOKENS " (string-join (reverse toks) " ")))))
  (and (logging-enabled?)
       (let loop ([strs strs] [line '()])
         (cond
           [(null? strs) (line! line)]
           [(member (car strs) '("newline" "blank"))
            (line! (cons (car strs) line))
            (loop (cdr strs) '())]
           [else (loop (cdr strs) (cons (car strs) line))]))))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Printer phase
;;
;; (log-printer FMT ARG ...) — a plain format string; the printer does its own layout.
;;
;; PRT space               col=21 ind=2 pend=space  cluster=12

(define-syntax (log-printer stx)
  (syntax-case stx ()
    [(_ FMT ARG ...)
     #'(and (logging-enabled?)
            (log! 'printer (string-append "PRT " (format FMT ARG ...))))]))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Formatting

;; Symbols are shown bare; everything else as it would be written
(define ($val v)
  (if (symbol? v) (symbol->string v) (format "~v" v)))

(define ($vars names vals)
  (string-join (for/list ([n (in-list names)] [v (in-list vals)])
                 (format "~a=~a" n ($val v)))
               " "))
