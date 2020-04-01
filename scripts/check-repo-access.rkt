#lang racket/base

;; Checks that a given user has write access to all the repos involved
;; in the release.

(require racket/cmdline
         racket/match
         "private/util.rkt"
         "private/github.rkt")

;; given a username, a catalog, and whether or not to be verbose,
;; check to make sure that the collaborators list for each repo in
;; the catalog can be checked and lists the given user as having
;; write access
(define (check-repo-access who catalog verbose?)
  (define fail? #f)
  (define (no-write-access repo-owner repo)
    (set! fail? #t)
    (eprintf "! no write access to ~a/~a\n" repo-owner repo))
  (displayln "Checking access to repos....")
  (for ([(user+repo checksum) (in-hash (get-sources catalog))])
    (match user+repo
      [(list repo-owner repo)
       (when verbose?
         (printf ". ~v\n" repo))
       (define contributors
         (get/github
          (format "https://api.github.com/repos/~a/~a/collaborators?per_page=~a"
                  repo-owner repo results-per-page)
          #:credential-style 'user
          #:fail (lambda _
                   (no-write-access repo-owner repo)
                   '())))
       (when (not (member who (for/list ([c (in-list contributors)])
                                (hash-ref c 'login))))
         (no-write-access repo-owner repo))]))
  (unless fail?
    (displayln "You have access to all release-relevant repos.")))

;; ------------------------------------------------------------

(module+ main
  (define *src-dir (current-directory))
  (define *verbose? #f)
  (check-repo-access
   (command-line
    #:once-each
    [("-i" "--in") src-dir "Read catalog from src-dir" (set! *src-dir src-dir)]
    [("-v" "--verbose") "Print repo names as they're checked" (set! *verbose? #t)]
    #:args (username) username)
   (read-catalog *src-dir)
   *verbose?))
