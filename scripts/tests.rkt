#lang racket/base

(module+ test

  (require rackunit
           racket/set
           "poll.rkt"
           "private/github.rkt")

  ;; to test this, I guess I really want a repo that's guaranteed to have more than
  ;; 30 branches....
  (check-not-exn
   (λ ()
     (poll-source** "racket" "typed-racket" "branches")))

  ;; link header can be #f
  (check-not-exn
   (λ ()
     (poll-source** "racket" "simple-tree-text-markup" "branches")))
  

  ;; example from RFC 2617:
  ;; FIXME put these tests in a separate file to remove dependency on rackunit...
  (check-equal? (basic-auth-line "Aladdin" "open sesame")
                  "Authorization: Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==")

  (check-not-exn
   (λ () (get/github "https://api.github.com/repos/racket/db/branches")))
  (check-equal? (tag-exists? "racket" "racket" "v8.3") #t)
  
  (define-values (evens odds)
    (set-partition even? (set 3 24 -242 33 9 12)))
  
  (check-equal? evens (set 24 -242 12))
  (check-equal? odds (set 3 33 9))
  )
