#lang racket/base

;; List people who have contributed commits since the last release.

(require racket/cmdline
         racket/list
         racket/match
         racket/set
         json
         "private/util.rkt"
         "private/github.rkt")

;; get-contributors : Catalog -> Void
(define (get-contributors catalog since-tag)
  (define all-contributors
    (for/fold ([all-contributors (set)])
        ([(user+repo checksum) (in-hash (get-sources catalog))])
      (match user+repo
        [(list user repo)
         (define per-page 250) ; github API says so
         (define-values (commits-since-last-release _1 _2)
           (for/fold ([commits '()] ; order doesn't matter
                      [end   checksum]
                      [done?   #f])
               ([page (in-naturals 1)]
                #:break done?)
             (define res
               (hash-ref
                (get/github
                 (format "https://api.github.com/repos/~a/~a/compare/~a...~a?per_page=~a;page=~a"
                         user repo since-tag end per-page page)
                 #:handle read-json
                 #:fail (lambda _
                          ;; fails on jeapostrophe/racket-cheat
                          (eprintf "! failed to get commits for ~a/~a\n"
                                   user repo)
                          ;; safe to ignore and keep going
                          (hash 'commits '())))
                'commits))
             (define d? (not (= (length res) per-page)))
             (values (append res commits)
                     (and (not d?)
                          (hash-ref (first res) 'sha)) ; rest of range
                     d?)))
         (define contributor-names
           (for/set ([c commits-since-last-release])
             (hash-ref (hash-ref (hash-ref c 'commit) 'author) 'name)))
         (set-union all-contributors contributor-names)])))
  (for-each displayln (sort (set->list all-contributors) string-ci<?)))

;; ------------------------------------------------------------

(define (command:get-contributors args)
  (define *src-dir (current-directory))
  (define *since-tag #f)
  (command-line
   #:argv args
   #:once-each
   [("-i" "--in") src-dir "Read catalog from src-dir" (set! *src-dir src-dir)]
   [("--since-tag") tag-name "Tag for last release" (set! *since-tag tag-name)])
  (unless *since-tag
    (error 'add-tags "tag name required"))
  (get-contributors (read-catalog *src-dir) *since-tag))

(module+ main
  (command:get-contributors (current-command-line-arguments)))
