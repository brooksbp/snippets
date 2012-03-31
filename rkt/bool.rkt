#lang racket
(require redex)

;; (define x expression)     -variable definition
;; (define (f x) expression) -function definition f-function name, x-argument
;; (f 19/3)                  -function f applied to 19/3
;; +, expt, <, list...       -primitive operations. applied in prefix style
;; (define-struct name (field ...)) - structure definition.
;;                            maker function: make-name
;;                            predicate for values: name?
;;                            selector for each field value: name-field

;; S-expression are either: a number: 42, a boolean: #t #f, a symbol 'hello,
;;                          or a list of S-expr constructed with cons and null:
;;                          (cons 'f (cons 2 null)) === (list 'f 2)
;; lor === logical or === \/

(define-language bool-any-lang
  [B true       ;; boolean expr
     false
     (lor B B)]
  [C (lor C B)   ;; contexts
     (lor B C)
     hole])

;; following quoted S-expressions are legit B expressions
(define B1 (term true))
(define B2 (term false))
(define B3 (term (lor true false)))
(define B4 (term (lor B1 B2)))
(define B5 (term (lor false B4)))


;; legit C expressions
(define C1 (term (hole)))
(define C2 (term (lor (lor false false) hole)))
(define C3 (term (lor hole true)))

;; in-hole
(define IH (term (in-hole C true)))

;; match a pattern with an expression
(redex-match bool-any-lang
             B
             (term (lor false true)))

(redex-match bool-any-lang
             (in-hole C (lor true B))
             (term (lor true (lor true false))))

;; reduction relations
(define bool-any-red
  (reduction-relation
   bool-any-lang
   (--> (in-hole C (lor true B))
        (in-hole C true)
        lor-true)
   (--> (in-hole C (lor false B))
        (in-hole C B)
        lor-false)))

(redex-match bool-any-lang
             (in-hole C (lor true B))
             (term (lor (lor true (lor false true)) false)))

;; tracing
(traces bool-any-red
        (term (lor (lor true false) (lor true true))))

;; restricted reductions
(define-language bool-standard-lang
  [B true
     false
     (lor B B)]
  [E (lor E B)
     hole])

(define bool-standard-red
  (reduction-relation
   bool-standard-lang
   (--> (in-hole E (lor true B))
        (in-hole E true)
        lor-true)
   (--> (in-hole E (lor false B))
        (in-hole E B)
        lor-false)))

(traces bool-standard-red
        (term (lor (lor true false) (lor true true))))