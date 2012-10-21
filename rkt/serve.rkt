#lang racket

(require xml net/url racket/control)

(define (serve port-no)
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
    (define listener (tcp-listen port-no 5 #t))
    (define (loop)
      (accept-and-handle listener)
      (loop))
    (thread loop))
  (lambda ()
    (custodian-shutdown-all main-cust)))

(define (accept-and-handle listener)
  (define cust (make-custodian))
  (custodian-limit-memory cust (* 50 1024 1024))
  (parameterize ([current-custodian cust])
    (define-values (in out) (tcp-accept listener))
    (thread (lambda ()
              (handle in out)
              (close-input-port in)
              (close-output-port out))))
  (thread (lambda ()
            (sleep 10)
            (custodian-shutdown-all cust))))

(define (handle in out)
  (define req
    ; match the first line to extract the request:
    (regexp-match #rx"^GET (.+) HTTP/[0-9]+\\.[0-9]+"
                  (read-line in)))
  (when req
    ; discard the rest of the header:
    (regexp-match #rx"(\r\n|^)\r\n" in)
    ; dispatch:
    (let ([xexpr (prompt (dispatch (list-ref req 1)))])
      ; send reply:
      (display "HTTP/1.0 200 Okay\r\n" out)
      (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
      (display (xexpr->string xexpr) out))))

(define (dispatch str-path)
  (define url (string->url str-path))
  (define path (map path/param-path (url-path url)))
  (define h (hash-ref dispatch-table (car path) #f))
  (if h
      (h (url-query url))
      `(html (head (title "Error"))
             (body
              (font ((color "red")) "unknown: " ,str-path)))))

(define dispatch-table (make-hash))

(hash-set! dispatch-table "hello"
           (lambda (query)
             `(html (body "Hello, world!"))))

(define (build-request-page label next-url hidden)
  `(html
    (head (title "Enter a Number to Add"))
    (body ([bgcolor "white"])
          (form ([action ,next-url] [method "get"])
                ,label
                (input ([type "text"] [name "number"]
                        [value ""]))
                (input ([type "hidden"] [name "hidden"]
                        [value ,hidden]))
                (input ([type "submit"] [name "enter"]
                        [value "Enter"]))))))

(define (many query)
  (build-request-page "Number of greetings:" "/reply" ""))
(define (reply query)
  (define n (string->number (cdr (assq 'number query))))
  `(html (body ,@(for/list ([i (in-range n)])
                           " hello"))))
(hash-set! dispatch-table "many" many)
(hash-set! dispatch-table "reply" reply)

(define (sum query)
  (build-request-page "First number:" "/one" ""))
(define (one query)
  (build-request-page "Second number:" "/two" (cdr (assq 'number query))))
(define (two query)
  (let ([n (string->number (cdr (assq 'hidden query)))]
        [m (string->number (cdr (assq 'number query)))])
    `(html (body "The sum is " ,(number->string (+ m n))))))
(hash-set! dispatch-table "sum" sum)
(hash-set! dispatch-table "one" one)
(hash-set! dispatch-table "two" two)


(define (sum2 query)
  (define m (get-number "first number:"))
  (define n (get-number "second number:"))
    `(html (body "The sum is " ,(number->string (+ m n)))))

(hash-set! dispatch-table "sum2" sum2)

(define (get-number label)
  (define query
    (send/suspend
     (lambda (k-url)
       (build-request-page label k-url ""))))
  (string->number (cdr (assq 'number query))))

(define (send/suspend mk-page)
  (let/cc k
    (define tag (format "k~a" (current-inexact-milliseconds)))
    (hash-set! dispatch-table tag k)
    (abort (mk-page (string-append "/" tag)))))
