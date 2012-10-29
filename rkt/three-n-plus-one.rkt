#lang racket

; 3n+1 problem
;
; n > 0
; cycle-length(n) = iterations of the following, until n=1:
;                         (odd? n)  n = 3n+1
;                         (even? n) n = n/2
;
; Given cycle-length(n) and two numbers i and j, compute the max
; cycle-length over all numbers between i and j inclusive.

; http://blog.racket-lang.org/2012/10/the-3n1-problem_4990.html

(define (cycle-length n)
  (cond
    [(= n 1)
     1]
    [(odd? n)
     (add1 (cycle-length (add1 (* 3 n))))]
    [(even? n)
     (add1 (cycle-length (/ n 2)))]))

; > (time (for ([i (in-range 1 100000)])
;           (cycle-length i)))
; cpu time: 2192 real time: 2195 gc time: 0

; accumulator instead of add1 ?
(define (cycle-length2 n)
  (cycle-length/acc n 1))

(define (cycle-length/acc n acc)
  (cond
    [(= n 1)
     acc]
    [(odd? n)
     (cycle-length/acc (add1 (* 3 n)) (add1 acc))]
    [(even? n)
     (cycle-length/acc (/ n 2) (add1 acc))]))

; > (time (for ([i (in-range 1 100000)])
;           (cycle-length2 i)))
; cpu time: 2056 real time: 2058 gc time: 0

; memoization?
(define table (make-hash))

(define (cycle-length3 n)
  (cond
    [(hash-has-key? table n)
     (hash-ref table n)]
    [else
     (define answer
       (cond
         [(= n 1)
          1]
         [(odd? n)
          (add1 (cycle-length3 (add1 (* 3 n))))]
         [(even? n)
          (add1 (cycle-length3 (/ n 2)))]))
     (hash-set! table n answer)
     answer]))

; > (time (for ([i (in-range 1 100000)])
;           (cycle-length3 i)))
; cpu time: 228 real time: 230 gc time: 36

; memoization as rewrite rule:
(define-syntax-rule (define/memo (name id) body ...)
  (begin
    (define table (make-hash))
    (define (name id)
      (cond
        [(hash-has-key? table id)
         (hash-ref table id)]
        [else
         (define answer (begin body ...))
         (hash-set! table id answer)
         answer]))))

(define/memo (cycle-length4 n)
  (cond
    [(= n 1)
     1]
    [(odd? n)
     (add1 (cycle-length4 (add1 (* 3 n))))]
    [(even? n)
     (add1 (cycle-length4 (/ n 2)))]))

; max cycle-length in range
(define (max-cycle-length-range i j)
  (apply max
         (for/list ([n (in-range i (add1 j))])
           (cycle-length4 n))))

(define-syntax (for/max stx)
  (syntax-case stx ()
    [(_ clauses . defs+exprs)
     (with-syntax ([original stx])
       #'(for/fold/derived original
                           ([current-max -inf.0])
                           clauses
            (define maybe-new-max
              (let () . defs+exprs))
            (if (> maybe-new-max current-max)
                maybe-new-max
                current-max)))]))

(define (max-cycle-length-range2 i j)
  (for/max ([n (in-range i (add1 j))])
           (cycle-length4 n)))

















































