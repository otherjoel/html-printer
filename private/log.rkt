#lang racket/base

(require racket/logging)

(provide (all-defined-out))

(module+ test)

(define-logger html-writer)

(define (log-debug . items)
  (log-html-writer-debug (apply format items)))

(define (log-err . items)
  (log-html-writer-error (apply format items)))

(define (log-msg . items)
  (log-html-writer-info (apply format items)))

(define (logging-to-stderr proc)
  (with-logging-to-port (current-error-port) proc #:logger html-writer-logger 'debug 'html-writer))
