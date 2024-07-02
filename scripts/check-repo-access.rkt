#lang racket/base

;; Checks that a given user has write access to all the repos involved
;; in the release.

(require racket/cmdline
         racket/match
         racket/file
         "private/util.rkt"
         "private/github.rkt")

;; given a username, a catalog, and whether or not to be verbose,
;; check to make sure that the collaborators list for each repo in
;; the catalog can be checked and lists the given user as having
;; write access
(define (check-catalog-repos-access who catalog verbose?)
  (define owner+repos (hash-keys (get-sources catalog)))
  (define fail? (check-repos-access who owner+repos verbose?))
  (unless fail?
    (displayln "You have access to all release-catalog repos.")))

;; given a username, a list of owner/repo lists, and whether or not to be verbose,
;; check to make sure that the collaborators list for each repo in
;; the catalog can be checked and lists the given user as having
;; write access
(define (check-aux-repos-access who owner+repos verbose?)
  (define fail? (check-repos-access who owner+repos verbose?))
  (unless fail?
    (displayln "You have access to all auxiliary repos.")))

;; given a username, a list of owner/repo lists, and whether to be verbose,
;; check write access to the given repos. NB: this is all totally
;; github-specific. Returns #t if a failure has occurred
(define (check-repos-access who owner+repos verbose?)
  (define fail? #f)
  (define (no-write-access repo-owner repo)
    (set! fail? #t)
    (eprintf "! no write access to ~a/~a\n" repo-owner repo))
  (displayln "Checking access to repos....")
  (for ([owner+repo owner+repos])
    (match owner+repo
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
  fail?)

;; given a directory, look in the given directory for a file formatted as e.g.
#|https://github.com/stamourv/optimization-coach
https://github.com/jeapostrophe/racket-cheat
https://github.com/RenaissanceBug/racket-cookies
https://github.com/Metaxal/quickscript
https://github.com/Metaxal/quickscript-test
https://github.com/rmculpepper/repo-manager-app
|#
;; ... and return a list of 2-element lists containing owner and repo names
(define (read-aux-repos src-dir)
  (define file-str (file->string (build-path src-dir "auxiliary-repos.txt")))
  (map
   (λ (l)
     (match l
       [(regexp #px"^https://github.com/(.*)/(.*)$" (list _ owner repo))
        (list owner repo)]))
   (filter
    (λ (l) (not (equal? l "")))
    (regexp-split #px"\n" file-str))))

;; ------------------------------------------------------------

(module+ main
  (define *src-dir (current-directory))
  (define *catalog? #t)
  (define *verbose? #f)
  (define who
    (command-line
     #:once-each
     [("-i" "--in") src-dir "Read catalog/list from src-dir" (set! *src-dir src-dir)]
     [("-v" "--verbose") "Print repo names as they're checked" (set! *verbose? #t)]
     #:once-any
     [("-c" "--catalog") "Read from a catalog (the default)" (set! *catalog? #t)]
     [("-r" "--repo-list") "Read from a repo-list named auxiliary-repos.txt" (set! *catalog? #f)]
     #:args (username) username))
  (cond [*catalog?
         (check-catalog-repos-access who (read-catalog *src-dir) *verbose?)]
        [else
         (check-aux-repos-access who (read-aux-repos *src-dir) *verbose?)]))
