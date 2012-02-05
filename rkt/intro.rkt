#lang racket

;; Top pane of DrRacket is `definitions area'.

;; To load file from command-line `racket' use:
;; (enter! "file.rkt")

(define (extract str)
  (substring str 4 7))

; > (extract "the boy")
; "boy"

;; Or, evaluate right here when definitions load
;(extract "the boy")

;; Create executables
;; - DrRacket: Racket -> Create Executable
;; - raco exe <src-filename>
;; - #! /usr/bin/env racket
;;   chmod +x <filename>

; 1 1/2 1+2i 3.14 6.02e+23 - numbers
; #t #f - booleans
; "strings \"yeah\"" - unicode strings

;; A program module is written as
; #lang <langname> <topform>*
;; where <topform> is either a <definition> or an <expr>

(define pie 3)
(define (piece str)
  (substring str 0 pie))

; > pie
; 3
; > (piece "key lime")
; "key"

;; Definitions can include multiple expressions in body.
;; The value of the last expression is returned. Other
;; expressions are evaluated usually for side-effects.

(define (bake flavor)
  (printf "pre-heating oven...\n")
  (string-append flavor " pie"))

; > (bake "apple")
; pre-heating oven...
; "apple pie"

; () [] {} " , ' ` ; # | \  - identifiers

; substring, string-append  - function identifiers

(define (reply s)
  (if (equal? "hello" (substring s 0 5))
      "hi!"
      "huh?"))

; > (reply "hello racket!")
; "hi!"
; > (reply "wassup")
; "huh?"

(define (reply-check s)
  (if (and (string? s)
           (>= (string-length s) 5)
           (equal? "hello" (substring s 0 5)))
      "hi!"
      "huh?"))

(define (reply-more s)
  (cond
    [(and (>= (string-length s) 5)
          (equal? "hello" (substring s 0 5)))
     "hi!"]
    [(and (>= (string-length s) 7)
          (equal? "goodbye" (substring s 0 7)))
     "bye!"]
    [(equal? "?" (substring s (- (string-length s) 1)))
     "I don't know"]
    [else "huh?"]))

(define (double v)
  ((if (string? v)
       string-append
       +)
   v v))

;; Anonymous functions with `lambda'

(define (twice f v)
  (f (f v)))

; > (twice sqrt 16)
; 2

(define (louder s)
  (string-append s "!"))
; > (twice louder "hello")
; "hello!!"

; > (twice (lambda (s) (string-append s "!"))
;          "hello")
; "hello!!"

(define (converse s)
  (define (starts? s2)  ; local to converse
    (define len2 (string-length s2))  ; local to starts?
    (and (>= (string-length s) len2)
         (equal? s2 (substring s 0 len2))))
  (cond
    [(starts? "hello") "hi!"]
    [(starts? "goodbye") "bye!"]
    [else "huh?"]))

;; Use `let*' when a binding needs to use an earlier binding
;; in the same let expr































