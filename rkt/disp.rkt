#lang racket

(require web-server/dispatch
         web-server/http
         net/url)

(define-values (go-dispatch u)
  (dispatch-rules
   [("") index]
   [("user" (string-arg)) user]
   [("post" (integer-arg) (integer-arg)) post]
   [else index]))

(define (index request)
  `(index))
(define (user request user-name)
  `(user ,user-name))
(define (post request int1 int2)
  `(post ,int1 ,int2))

(define (url-request u)
  (make-request #"GET" (string->url u) empty
                (delay empty) #f "0.0.0.0" 80 "0.0.0.0"))

; > (go-dispatch
;    (url-request "http://www.lol.com"))
; '(index)
; > (go-dispatch
;    (url-request "http://www.lol.com/user/brian"))
; '(user "brian")
; > (go-dispatch
;    (url-request "http://www.lol.com/user/brian/lol"))
; '(index)
; > (go-dispatch
;    (url-request "http://www.lol.com/post/2011/3/"))
; '(index)
; > (go-dispatch
;    (url-request "http://www.lol.com/post/2011/3"))
; '(post 2011 3)


; Generate URLs

; > (u index)
; "/"
; > (u user "brian")
; "/user/brian"
; > (u post 2011 4)
; "/post/2011/4"