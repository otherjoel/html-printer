#lang racket/base

(require racket/list
         racket/match
         racket/string)

(provide try-extract-version version>=?)

(define (try-extract-version str)
  (and (string? str)
       (match (regexp-match semver-regex-anywhere str)
         [(? list? l) (car l)]
         [_ #f])))

(define (version>=? ver1 ver2)
  (with-handlers ([exn:fail:contract? (λ (_) #false)])
    (match (semver-version-compare (parse-semver-version ver1)
                                   (parse-semver-version ver2))
      [(or 0 1) #true]
      [_ #false])))

;; Stripped-down non-typed version of Alexis King's semver package.
;; Dropped in here to avoid extra dependencies on semver and alexis-util.
;; https://github.com/lexi-lambda/racket-semver/blob/fee107ee2401b5f7d7d797258eab3062ddb71232/semver/main.rkt

#|
Copyright 2015 Alexis King

Permission to use, copy, modify, and/or distribute this software for any purpose
with or without fee is hereby granted, provided that the above copyright notice
and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
THIS SOFTWARE.
|#

(define _semver-regex
  (string-append
   "(\\d+)\\.(\\d+)\\.(\\d+)"  ; digits
   "(?:-([0-9A-Za-z\\-]+"       ; suffix start
   "(?:\\.[0-9A-Za-z\\-]+)*))?" ; rest of suffix
   "(?:\\+[0-9A-Za-z\\-]+"      ; build metadata start
   "(?:\\.[0-9A-Za-z\\-]+)*)?" ; rest of build metadata
   ))

(define semver-regex-anywhere
  (pregexp _semver-regex))

(define semver-regex
  (pregexp (string-append "^" _semver-regex "$")))

(struct SemverVersion (major minor patch suffix) #:transparent)

; Utility function to parse strings to integers.
(define (string->integer s)
  (cond
    [(not s) #f]
    [else
     (define i (string->number s))
     (if (exact-integer? i) i #f)]))

; Compares two integers
(define (integer-compare i1 i2)
  (cond
    [(= i1 i2) 0]
    [(> i1 i2) 1]
    [else     -1]))

; Compares two semver suffixes
(define (semver-suffix-compare s1 s2)
  (match (list s1 s2)
    ['(() ()) 0]
    [(list _ '()) 1]
    [(list '() _) -1]
    [(list (list (? string?) ___) (list (? exact-integer?) ___)) 1]
    [(list (list (? exact-integer?) ___) (list (? string?) ___)) -1]
    [(list (list-rest v1 rest1) (list-rest v2 rest2))
     (cond
       [(exact-integer? v1)
        (cond
          [(> v1 v2) 1]
          [(< v1 v2) -1]
          [else (semver-suffix-compare rest1 rest2)])]
       [else
        (cond
          [(string>? v1 v2) 1]
          [(string<? v1 v2) -1]
          [else (semver-suffix-compare rest1 rest2)])])
     ]))

; Compares two semver versions
(define (semver-version-compare v1 v2)
  (let ([major1 (SemverVersion-major v1)]
        [major2 (SemverVersion-major v2)]
        [minor1 (SemverVersion-minor v1)]
        [minor2 (SemverVersion-minor v2)]
        [patch1 (SemverVersion-patch v1)]
        [patch2 (SemverVersion-patch v2)]
        [suffix1 (SemverVersion-suffix v1)]
        [suffix2 (SemverVersion-suffix v2)])
    (if (not (= major1 major2)) (integer-compare major1 major2)
        (if (not (= minor1 minor2)) (integer-compare minor1 minor2)
            (if (not (= patch1 patch2)) (integer-compare patch1 patch2)
                (match (list suffix1 suffix2)
                  [(list '() '()) 0]
                  [(list '() _) 1]
                  [(list _ '()) -1]
                  [else (semver-suffix-compare suffix1 suffix2)]))))))

; Checks if a given string is a valid semver version.
(define (semver-version? str)
  (regexp-match? semver-regex str))

; Parses a possible semver suffix to a list of the dot-separated components.
(define (parse-semver-suffix str)
  (cond
    [(not str) '()]
    [else
     (define split (string-split str "."))
     (for/list ([x (in-list split)])
       (or (string->integer x) x))]))

; Parses a string to a semver.
(define (parse-semver-version str)
  (define data (regexp-match semver-regex str))
  (cond
    [data
     (SemverVersion (string->integer (second data))
                    (string->integer (third data))
                    (string->integer (fourth data))
                    (parse-semver-suffix (fifth data)))]
    [else (raise-argument-error 'parse-semver-version "semver-version?" str)]))
