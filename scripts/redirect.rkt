#lang racket/base
(require racket/match
         racket/pretty
         racket/cmdline
         data/order
         "private/util.rkt")
(provide (all-defined-out))

;; For each pkg in the catalog matching <from-user>/<from-repo>#<from-branch>,
;; redirect the source to #<branch>.

;; ============================================================
;; Redirect branch/tag of selected pkg sources

;; redirected-repos : Hash[ (list user-string repo-string) => #t ]
(define redirected-repos (make-parameter #f))

;; redirect-catalog : Catalog String ... -> Catalog
(define (redirect-catalog catalog to-branch
                          #:from-user [from-user #f]
                          #:from-repo [from-repo #f]
                          #:from-branch [from-branch #f])
  (eprintf "! Redirecting catalog to branch: ~s\n" to-branch)
  (define redirect (make-redirect-url from-user from-repo from-branch to-branch))
  (parameterize ((redirected-repos (make-hash)))
    (define new-catalog
      (for/hash ([(name info) (in-hash catalog)])
        (check-minimized name info)
        ;; (eprintf "! Processing pkg: ~s\n" name)
        (values name (redirect-pkg name info redirect))))
    (values new-catalog
            (sort (hash-keys (redirected-repos))
                  (order-<? datum-order)))))

;; redirect-pkg : String InfoHash (String -> String) -> InfoHash
(define (redirect-pkg name info0 redirect)
  (define info (hash-copy info0))
  (define old-source (hash-ref info 'source))
  (define new-source (redirect old-source))
  (hash-set! info 'source new-source)
  info)

;; make-redirect-url : String/#f String/#f String/#f String -> String -> String
(define ((make-redirect-url from-user from-repo from-branch to-branch) url)
  (define (skip [why ""])
    ;; (eprintf "! Not redirecting~a ~s\n" why url)
    url)
  (match (parse-repo-url url)
    [(list user repo branch path)
     (cond [(and from-user (not (equal? user from-user)))
            (skip " (user doesn't match)")]
           [(and from-repo (not (equal? repo from-repo)))
            (skip " (repo doesn't match)")]
           [(and from-branch (not (equal? (or branch "master") from-branch)))
            (skip " (branch doesn't match)")]
           [else
            (eprintf "! Redirecting ~a\n" url)
            (hash-set! (redirected-repos) (list user repo) #t)
            (make-git-source user repo path to-branch)])]
    ['native
     ;; Skip silently
     url]
    [#f
     (skip " (not a github repo)")]))

;; print-redirected-repos : (Listof (List String String)) -> Void
(define (print-redirected-repos repos)
  (define repos* (for/list ([repo repos]) (format "~a/~a" (car repo) (cadr repo))))
  (define repos** (sort repos* string<?))
  (for ([repo repos**])
    (printf "~a\n" repo)))

;; ------------------------------------------------------------

(define (command:branch args)
  (define *force? #f)
  (define *src-dir (current-directory))
  (define *dest-dir #f)
  (define *branch "master")
  (define *from-user #f)
  (define *from-repo #f)
  (define *from-branch #f)
  (command-line
   #:argv args
   #:once-each
   [("-f" "--force") "Overwrite contents of dest-dir" (set! *force? #t)]
   [("-i" "--in") src-dir "Read catalog from src-dir" (set! *src-dir src-dir)]
   [("-o" "--out") dest-dir "Write catalog to dest-dir" (set! *dest-dir dest-dir)]
   [("-b" "--branch") branch "Redirect pkg repos to branch" (set! *branch branch)]
   [("--from-user") from-user "Only packages with given user" (set! *from-user from-user)]
   [("--from-repo") from-repo "Only packages with given repo name" (set! *from-repo from-repo)]
   [("--from-branch") from-branch "Only packages on given branch" (set! *from-branch from-branch)])
  (set! *dest-dir (normalize-dest *src-dir *dest-dir *force?))
  (copy-catalog* *src-dir *dest-dir *force?
                 (lambda (catalog) (redirect-catalog catalog *branch
                                                #:from-user *from-user
                                                #:from-repo *from-repo
                                                #:from-branch *from-branch))
                 (lambda (redirected-repos)
                   (call/write-log *dest-dir "branch-redirected-repos"
                                   (lambda () (pretty-write redirected-repos))))))

(module+ main
  (command:branch (current-command-line-arguments)))
