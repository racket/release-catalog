#lang racket/base
(require racket/match
         racket/date
         racket/cmdline
         racket/port
         json
         "private/util.rkt"
         "private/github.rkt")
(provide (all-defined-out))

;; For each src in the catalog matching <from-user>/*#<from-branch>,
;; delete the branch <branch> if it exists.

;; delete-branches : Catalog String -> Void
(define (delete-branches catalog branch from-user from-branch)
  (for ([user+repo (in-hash-keys (get-sources catalog from-user from-branch))])
    (match user+repo
      [(list user repo)
       (delete-branch user repo branch)])))

;; get-sources : Catalog String/#f String -> Hash[ (List String String) => String ]
(define (get-sources catalog from-user from-branch)
  ;; repos : Hash[ (List String String) => String ]
  (define repos (make-hash))
  (for ([(pkg-name info) (in-hash catalog)])
    (define src (hash-ref info 'source))
    (match (parse-repo-url src)
      [(list user repo branch path)
       (when (and (or (not from-user) (equal? user from-user))
                  (or (not from-branch) (equal? branch from-branch)))
         ;; Assume checksums consistent for all srcs w/ user/repo.
         (hash-set! repos (list user repo) (hash-ref info 'checksum)))]
      [_ (void)]))
  repos)

(define (delete-branch user repo branch-name)
  (cond [(branch-exists? user repo branch-name)
         (define delete-result
           (delete/github (format "https://api.github.com/repos/~a/~a/git/refs/heads/~a"
                                  user repo branch-name)
                          #:credential-style 'user))
         (eprintf "! Deleted branch ~s from ~a/~a\n" branch-name user repo)]
        [else
         (eprintf "! No branch ~s exists in ~a/~a\n" branch-name user repo)]))

;; branch-exists? : String String String -> Boolean
(define (branch-exists? user repo branch-name)
  (define result
    (get/github (format "https://api.github.com/repos/~a/~a/git/refs/heads/~a"
                        user repo branch-name)
                #:fail (lambda (response-header in) #f)))
  (and result #t))

;; ------------------------------------------------------------

(define (command:delete-branches args)
  (define *src-dir (current-directory))
  (define *branch #f)
  (define *from-user #f)
  (define *from-branch #f)
  (command-line
   #:argv args
   #:once-each
   [("-f" "--force") "Does nothing" (void)]
   [("-i" "--in") src-dir "Read catalog from src-dir" (set! *src-dir src-dir)]
   [("--branch") branch "Branch to delete" (set! *branch branch)]
   [("--from-user") from-user "For pkgs with source from user"
    (set! *from-user from-user)]
   [("--from-branch") from-branch "For pkgs with source from branch"
    (set! *from-branch from-branch)])
  (unless *branch
    (error 'delete-branches "must specify a branch to delete"))
  (delete-branches (read-catalog *src-dir) *branch *from-user *from-branch))

(module+ main
  (command:delete-branches (current-command-line-arguments)))
