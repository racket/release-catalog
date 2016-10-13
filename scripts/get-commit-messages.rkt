#lang racket/base

;; List commit messages since the last release, on all release repos.
;; For release announcement composition purposes.

(require racket/cmdline
         racket/match
         racket/set
         json
         "private/util.rkt"
         "private/github.rkt")

;; get-commit-messages : Catalog Tag -> Void
(define (get-commit-messages catalog since-tag)
  (define all-commits (get-commits-since-last-release catalog since-tag))
  (for ([(repo commits) (in-hash all-commits)])
    (when (not (null? commits))
      (displayln (make-string 80 #\=))
      (displayln repo)
      (for ([c (in-list commits)])
        (define commit  (hash-ref c 'commit))
        (define author  (hash-ref commit 'author))
        (define message (hash-ref commit 'message))
        (displayln (make-string 80 #\-))
        (printf "~a ~a ~a\n"
                (hash-ref author 'name)
                (hash-ref author 'email)
                (hash-ref author 'date))
        (newline)
        (displayln message)
        (newline))
      (newline))))

;; ------------------------------------------------------------

(define (command:get-commit-messages args)
  (define *src-dir (current-directory))
  (define *since-tag #f)
  (command-line
   #:argv args
   #:once-each
   [("-i" "--in") src-dir "Read catalog from src-dir" (set! *src-dir src-dir)]
   [("--since-tag") tag-name "Tag for last release" (set! *since-tag tag-name)])
  (unless *since-tag
    (error 'add-tags "tag name required"))
  (get-commit-messages (read-catalog *src-dir) *since-tag))

(module+ main
  (command:get-commit-messages (current-command-line-arguments)))
