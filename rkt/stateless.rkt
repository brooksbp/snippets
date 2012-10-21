#lang web-server

(require web-server/servlet
         web-server/servlet-env)

(provide interface-version
         stuffer
         start)

(define interface-version 'stateless)

(define stuffer
  (stuffer-chain
   serialize-stuffer
   (md5-stuffer (build-path "/home/brian/proj/rktweb/urls"))))


(define (start request)
  (render-page request))

(define (render-page request)
  (local [(define (response-generator embed/url embed/input-hidden)
            (response/xexpr
             `(html (body
                     (h1 "blah")
                     (div ,(url->string embed/url))
                     (form ,embed/input-hidden))))))]
    (send/suspend/hidden response-generator)))


(serve/servlet start
               #:launch-browser? #t
               #:quit? #f
               #:stateless? #t
               #:stuffer stuffer
               #:listen-ip #f
               #:port 8080
               #:extra-files-paths
               (list (build-path "/home/brian/proj/rktweb/static/"))
               #:servlet-regexp #rx"")