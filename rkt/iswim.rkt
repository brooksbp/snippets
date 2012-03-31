#lang racket

(require redex)

(define-language iswim
  ((M N L K) X (lam X M) (M M) b (o2 M M) (o1 M))
  (o o1 o2)
  (o1 add1 sub1 iszero)
  (o2 + - * up)
  (b number)
  ((V U W) b X (lam X M))
  (E hole (V E) (E M) (o V ... E M ...))
  ((X Y Z) variable-not-otherwise-mentioned))

 (redex-match iswim (in-hole E number) (term (+ 1 3)))
 
 (define-metafunction iswim
   [(delta (iszero 0)) (lam x (lam y x))]
   [(delta (iszero b)) (lam x (lam y y))]
   [(delta (add1 b)) ,(add1 (term b))]
   [(delta (sub1 b)) ,(sub1 (term b))]
   [(delta (+ b_1 b_2)) ,(+ (term b_1) (term b_2))]
   [(delta (- b_1 b_2)) ,(- (term b_1) (term b_2))]
   [(delta (* b_1 b_2)) ,(* (term b_1) (term b_2))]
   [(delta (up b_1 b_2)) ,(expt (term b_1) (term b_2))])
 
 
 ;; capture-avoiding substitution function
 (define-metafunction iswim
   
   ;; 1. X_1 bound, so don't continue in lambda body
   [(subst (lam X_1 any_1) X_1 any_2)
    (lam X_1 any_1)]
   
   ;; 2. do capture avoiding substitution by generating a fresh name
   [(subst (lam X_1 any_1) X_2 any_2)
    (lam X_3
         (subst (subst-var any_1 X_1 X_3) X_2 any_2))
    (where X_3 ,(variable-not-in (term (X_2 any_1 any_2))
                                 (term X_1)))]
   
   ;; 3. replace X_1 with any_1
   [(subst X_1 X_1 any_1) any_1]
   
   ;; the last two cases just recur on the tree structure of the term
   [(subst (any_2 ...) X_1 any_1)
    ((subst any_2 X_1 any_1) ...)]
   [(subst any_2 X_1 any_1) any_2])
 
 ;; substitution function
 (define-metafunction iswim
   [(subst-var (any_1 ...) variable_1 variable_2)
    ((subst-var any_1 variable_1 variable_2) ...)]
   [(subst-var variable_1 variable_1 variable_2) variable_2]
   [(subst-var any_1 variable_1 variable_2) any_1])
 
 
 (define iswim-red
   (reduction-relation
    iswim
    (--> (in-hole E ((lam X M) V))
         (in-hole E (subst M X V))
         betav)
    (--> (in-hole E (o b ...))
         (in-hole E (delta (o b ...)))
         delta)))
 
 (traces iswim-red
         (term ((lam y (y y)) (lam x (x x)))))
 
 (define iswim-standard
   (reduction-relation
    iswim
    (v ((lam X M) V) (subst M X V) betav)
    (v (o b ...) (delta (o b ...)) delta)
    with
    [(--> (in-hole E M) (in-hole E N)) (v M N)]))
 
  (traces iswim-standard
         (term ((lam y (y y)) (lam x (x x)))))