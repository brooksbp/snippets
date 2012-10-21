#lang racket/base

(struct blog (posts) #:mutable)

(struct post (title body comments) #:mutable)

(define BLOG
  (blog
   (list (post "Second Post"
               "This is another post"
               (list))
         (post "First Post"
               "This is my first post"
               (list "First comment!")))))

(define (blog-insert-post! a-blog a-post)
  (set-blog-posts!
   a-blog
   (cons a-post (blog-posts a-blog))))

(define (post-insert-comment! a-post a-comment)
  (set-post-comments!
   a-post
   (append (post-comments a-post) (list a-comment))))

(provide (all-defined-out))

