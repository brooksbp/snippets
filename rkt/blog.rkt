#lang racket

(require web-server/servlet
         web-server/servlet-env
         web-server/formlets
         "model-3.rkt")
(provide/contract (start (request? . -> . response?)))

; start: request -> response
(define (start request)
    (render-blog-page 
     (initialize-blog! (build-path "/home/brian/proj/rktweb/the-blog-data-sql3.db"))
     request))

(define new-post-formlet
  (formlet
   (#%# ,{input-string . => . title}
        ,{input-string . => . body})
   (values title body)))

; render-blog-page: blog request -> response
; Consumes a blog and a request, and produces an HTML page
; of the content of the blog.
(define (render-blog-page a-blog request)
  (local [(define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "My Blog"))
                    (link ((rel "stylesheet")
                           (href "style.css")
                           (type "text/css")))
                    (body
                     (h1 "My Blog")
                     ,(render-posts a-blog embed/url)
                     (form ([action
                             ,(embed/url insert-post-handler)])
                           ,@(formlet-display new-post-formlet)
                           (input ([type "submit"])))))))
          
          (define (insert-post-handler request)
            (define-values (title body)
              (formlet-process new-post-formlet request))
            (blog-insert-post! a-blog title body)
            (render-blog-page a-blog (redirect/get)))]
            
    (send/suspend/dispatch response-generator)))

(define new-comment-formlet
  input-string)

(define (render-post-detail-page a-blog a-post request)
  (local [(define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Post Details"))
                    (body
                     (h1 "Post Details")
                     (h2 ,(post-title a-post))
                     (p ,(post-body a-post))
                     ,(render-as-itemized-list (post-comments a-post))
                     (form ([action
                             ,(embed/url insert-comment-handler)])
                           ,@(formlet-display new-comment-formlet)
                           (input ([type "submit"])))
                     (a ((href ,(embed/url back-handler)))
                        "back")))))
          
          (define (insert-comment-handler request)
            (render-confirm-add-comment-page
             a-blog
             (formlet-process new-comment-formlet request)
             a-post
             request))

          (define (back-handler request)
            (render-blog-page a-blog request))]
    
    (send/suspend/dispatch response-generator)))

(define (render-confirm-add-comment-page a-blog a-comment
                                         a-post request)
  (local [(define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Add a Comment"))
                    (body
                     (h1 "Add a Comment")
                     "The comment: " (div (p ,a-comment))
                     "will be added to "
                     (div ,(post-title a-post))
                     (p (a ((href ,(embed/url yes-handler))) "Yes!"))
                     (p (a ((href ,(embed/url cancel-handler))) "No!"))))))
          (define (yes-handler request) 
            (post-insert-comment! a-blog a-post a-comment)
            (render-post-detail-page a-blog a-post (redirect/get)))
          (define (cancel-handler request)
            (render-post-detail-page a-blog a-post request))]
    
    (send/suspend/dispatch response-generator)))

(define (render-post a-blog a-post embed/url)
  (local [(define (view-post-handler request)
            (render-post-detail-page a-blog a-post request))]
    `(div ((class "post"))
          (a ((href ,(embed/url view-post-handler)))
             ,(post-title a-post))
          (p ,(post-body a-post))
          (div ,(number->string (length (post-comments a-post)))
               " comment(s)"))))

(define (render-posts a-blog embed/url)
  (local [(define (render-post/embed/url a-post)
            (render-post a-blog a-post embed/url))]
    `(div ((class "posts"))
          ,@(map render-post/embed/url (blog-posts a-blog)))))


(define (render-as-itemized-list fragments)
  `(ul ,@(map render-as-item fragments)))

(define (render-as-item a-fragment)
  `(li ,a-fragment))

(serve/servlet start
               #:launch-browser? #f
               #:quit? #f
               #:listen-ip #f
               #:port 8080
               #:extra-files-paths
               (list (build-path "/home/brian/proj/rktweb/static/"))
               #:servlet-regexp #rx"")