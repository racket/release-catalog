#lang racket/base

;; List commit messages since the last release, on all release repos.
;; For release announcement composition purposes.

(require racket/cmdline
         racket/list
         racket/match
         racket/runtime-path
         racket/set
         racket/string
         racket/system
         json
         "private/util.rkt"
         "private/github.rkt")

;; there are many commits from the resyntax bot; these
;; should not appeear in the extracted list of commits
;; used to prepare the release.
(define authors-to-ignore '("resyntax-ci[bot]"))

;; get-commit-messages : Catalog Tag -> Void
(define (get-commit-messages catalog since-tag)
  (define sources (get-sources catalog))
  (for ([(user+repo commit) (in-hash sources)])
    (match-define (list user repo) user+repo)
    (define commits (get-repo-commits-since-last-release user repo commit since-tag))
    (when (not (null? commits))
      (displayln (make-string 80 #\=))
      (displayln user+repo)
      (define-values (ignored-commits interesting-commits)
        (partition (λ (c)
                     (member (hash-ref (hash-ref (hash-ref c 'commit) 'author) 'name)
                             authors-to-ignore))
                   commits))
      (printf "(ignoring ~v commits from authors (e.g. resyntax-ci[bot]) in ignore list)\n"
              (length ignored-commits))
      (for ([c (in-list interesting-commits)])
        (define commit  (hash-ref c 'commit))
        (define author  (hash-ref commit 'author))
        (define message (hash-ref commit 'message))
        (displayln (make-string 80 #\-))
        (printf "~a ~a ~a\n"
                (hash-ref author 'name)
                (hash-ref author 'email)
                (hash-ref author 'date))
        ;; the JSON doesn't have the raw sha...
        (define sha (last (string-split (hash-ref commit 'url) "/")))
        (displayln sha)
        (newline)
        (displayln message)
        (newline))
      (newline)
      (flush-output))))

;; ------------------------------------------------------------

(define-runtime-path here ".")

(define (command:get-commit-messages args)
  (define *src-dir here)
  (define *since-tag #f)
  (command-line
   #:argv args
   #:once-each
   [("-i" "--in") src-dir "Read catalog from src-dir" (set! *src-dir src-dir)]
   [("--since-tag") tag-name "Tag for last release" (set! *since-tag tag-name)])
  (unless *since-tag
    (error 'get-commit-messages "tag name required"))
  (parameterize ([current-directory here])
    (unless (system "git pull &> /dev/null")
      (error 'get-commit-messages "git pull failed")))
  (get-commit-messages (read-catalog *src-dir) *since-tag))

(module+ main
  (command:get-commit-messages (current-command-line-arguments)))
