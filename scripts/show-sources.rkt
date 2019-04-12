#lang racket/base
(require racket/match
         racket/cmdline
         "private/util.rkt")
(provide (all-defined-out))

;; For each <user>, <repo> such that some pkg in the catalog has
;; source <user>/<repo>/<from-branch>, print <user>/<repo> to stdout.

;; show-sources : Catalog -> Void
(define (show-sources catalog from-branch)
  (define repos (sort (get-sources catalog from-branch) string<?))
  (for ([repo (in-list repos)])
    (printf "~a\n" repo)))

;; get-sources : Catalog String/#f -> (Listof String)
;; given a catalog and an optional from-branch, return a list of the user/repo
;; pairs that appear in the catalog (with the given branch, if specified)
(define (get-sources catalog from-branch)
  ;; repos : Hash[ User/Repo-String => #t ]
  (define repos (make-hash))
  (for ([(pkg-name info) (in-hash catalog)])
    (define src (hash-ref info 'source))
    (match (parse-repo-url src)
      [(list user repo branch path)
       (when (or (not from-branch) (equal? branch from-branch))
         (hash-set! repos (format "~a/~a" user repo) #t))]
      [_ (void)]))
  (hash-keys repos))

;; ------------------------------------------------------------

(define (command:show-sources args)
  (define *from-branch #f)
  (define *src-dir (current-directory))
  (command-line
   #:argv args
   #:once-each
   [("-f" "--force") "No effect" (void)]
   [("-i" "--in") src-dir "Read catalog from src-dir"
    (set! *src-dir src-dir)]
   [("--from-branch") from-branch "Only show sources involving given branch"
    (set! *from-branch from-branch)])
  (show-sources (read-catalog *src-dir) *from-branch))

(module+ main
  (command:show-sources (current-command-line-arguments)))
