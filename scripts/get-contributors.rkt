#lang racket/base

;; List people who have contributed commits since the last release.

(require racket/cmdline
         racket/match
         racket/runtime-path
         racket/set
         racket/system
         json
         "private/util.rkt"
         "private/github.rkt")

;; get-contributors : Catalog Tag -> Void
(define (get-contributors catalog since-tag)
  (define all-contributors
    (for/fold ([all-contributors (set)])
        ([(repo commits-since-last-release)
          (in-hash (get-commits-since-last-release catalog since-tag))])
      (define contributor-names
        (for/set ([c commits-since-last-release])
          (hash-ref (hash-ref (hash-ref c 'commit) 'author) 'name)))
      (set-union all-contributors contributor-names)))
  (for-each displayln (sort (set->list all-contributors) string-ci<?)))

;; ------------------------------------------------------------

(define-runtime-path here ".")

(define (command:get-contributors args)
  (define *src-dir here)
  (define *since-tag #f)
  (command-line
   #:argv args
   #:once-each
   [("-i" "--in") src-dir "Read catalog from src-dir" (set! *src-dir src-dir)]
   [("--since-tag") tag-name "Tag for last release" (set! *since-tag tag-name)])
  (unless *since-tag
    (error 'get-contributors "tag name required"))
  (parameterize ([current-directory here])
    (unless (system "git pull")
      (error 'get-contributors "git pull failed")))
  (get-contributors (read-catalog *src-dir) *since-tag))

(module+ main
  (command:get-contributors (current-command-line-arguments)))
