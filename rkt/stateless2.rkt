#lang web-server
(require web-server/servlet-env)

;(define (start req)
;  (start
;   (send/suspend
;    (lambda (k-url)
;      (response/xexpr
;       `(html (body (a ([href ,k-url]) "Hello World!"))))))))

(define stuffer
  (stuffer-chain
   serialize-stuffer
   (md5-stuffer (build-path (find-system-path 'home-dir) ".urlz"))))

(define (start req)
  (start
   (send/suspend/hidden
    (lambda (k-url hidden-xexpr)
      (response/xexpr
       `(html (body
               (form ,hidden-xexpr)
               (a ([href ,(url->string k-url)]) "link"))))))))

(serve/servlet start
               #:stateless? #t
               #:stuffer stuffer)
