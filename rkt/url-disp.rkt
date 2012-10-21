#lang web-server/insta

(define (start request)
  (show-counter 0 request))

(define (show-counter n request)
  (local [(define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Counting example"))
                    (body
                     (a ((href ,(embed/url next-number-handler)))
                        ,(number->string n))))))
          (define (next-number-handler request)
            (show-counter (+ n 1) request))]
    (send/suspend/dispatch response-generator)))
