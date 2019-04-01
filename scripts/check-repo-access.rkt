#lang racket/base

;; Checks that a given user has write access to all the repos involved
;; in the release.

(require racket/cmdline
         racket/match
         "private/util.rkt"
         "private/github.rkt")


(define (check-repo-access who catalog)
  (define fail? #f)
  (define (no-write-access user repo)
    (set! fail? #t)
    (eprintf "! no write access to ~a/~a\n" user repo))
  (for ([(user+repo checksum) (in-hash (get-sources catalog))])
    (match user+repo
      [(list user repo)
       (define contributors
         (get/github
          (format "https://api.github.com/repos/~a/~a/collaborators?per_page=~a"
                  user repo results-per-page)
          #:credential-style 'user
          #:fail (lambda _
                   (no-write-access user repo)
                   '())))
       (unless (member who (for/list ([c (in-list contributors)])
                             (hash-ref c 'login)))
         (no-write-access user repo))]))
  (unless fail?
    (displayln "You have access to all release-relevant repos.")))

;; ------------------------------------------------------------

(module+ main
  (define *src-dir (current-directory))
  (check-repo-access
   (command-line
    #:once-each
    [("-i" "--in") src-dir "Read catalog from src-dir" (set! *src-dir src-dir)]
    #:args (username) username)
   (read-catalog *src-dir)))
