#lang racket/base
(require racket/list
         racket/local
         db)

(struct blog (db))

(struct post (blog id))

(define (initialize-blog! home)
  (define  db
    (virtual-connection
     (connection-pool
      (lambda () (sqlite3-connect #:database home #:mode 'create)))))
  (define the-blog (blog db))
  (unless (table-exists? db "posts")
    (query-exec db
                (string-append
                 "CREATE TABLE posts "
                 "(id INTEGER PRIMARY KEY, title TEXT, body TEXT)"))
    (blog-insert-post!
     the-blog "First Post" "This is my first post")
    (blog-insert-post!
     the-blog "Second Post" "This is another post"))
  (unless (table-exists? db "comments")
    (query-exec db
                "CREATE TABLE comments (pid INTEGER, content TEXT)")
    (post-insert-comment!
     the-blog (first (blog-posts the-blog))
     "First comment!"))
  the-blog)

(define (blog-posts a-blog)
  (local [(define (id->post an-id)
            (post a-blog an-id))]
    (map id->post
         (query-list
          (blog-db a-blog)
          "SELECT id FROM posts"))))

(define (post-title a-post)
  (query-value
   (blog-db (post-blog a-post))
   "SELECT title FROM posts WHERE id = ?"
   (post-id a-post)))

(define (post-body p)
  (query-value
   (blog-db (post-blog p))
   "SELECT body FROM posts WHERE id = ?"
   (post-id p)))

(define (post-comments p)
  (query-list
   (blog-db (post-blog p))
   "SELECT content FROM comments WHERE pid = ?"
   (post-id p)))

(define (blog-insert-post! a-blog title body)
  (query-exec
   (blog-db a-blog)
   "INSERT INTO posts (title, body) VALUES (?, ?)"
   title body))

(define (post-insert-comment! a-blog p a-comment)
  (query-exec
   (blog-db a-blog)
   "INSERT INTO comments (pid, content) VALUES (?, ?)"
   (post-id p) a-comment))

(provide blog? blog-posts
         post? post-title post-body post-comments
         initialize-blog!
         blog-insert-post!
         post-insert-comment!)
