#lang racket/base
(require racket/match
         racket/pretty
         racket/cmdline
         "checksums.rkt"
         "private/util.rkt"
         "private/github.rkt")
(provide (all-defined-out))

;; For each pkg in the catalog, check for a new release branch or for
;; an update to an existing release branch. In the case of a new
;; release branch, redirect the pkg source to the release branch and
;; update the checksum. In the case of a release branch update, update
;; the checksum.

;; ============================================================
;; Poll for new release branches

;; For each repository:
;; - if there is a release branch:
;;   - redirect 'source to release branch
;;   - update 'checksum
;; - otherwise, do nothing

(define only-repos (make-parameter #f))

;; poll-catalog : Catalog -> (values Catalog Hash[String => (cons String String)])
(define (poll-catalog catalog)
  (parameterize ((checksum-updates (make-hash)))
    ;; repo-cache : Hash[ (list String String) => RefsInfo ]
    (define repo-cache (make-hash))
    (define new-catalog
      (for/hash ([(name info) (in-hash catalog)])
        (check-minimized name info)
        (values name (or (poll-pkg info repo-cache) info))))
    (values new-catalog
            (for/hash ([(source old+new) (in-hash (checksum-updates))]
                       #:when (not (equal? (car old+new) (cadr old+new))))
              (values source old+new)))))

;; poll-pkg : InfoHash Cache -> InfoHash/#f
(define (poll-pkg info cache)
  (define source (hash-ref info 'source))
  (define old-checksum (hash-ref info 'checksum))
  (match (parse-repo-url source)
    [(list user repo old-branch path)
     (define refs-info (poll-source user repo cache))
     (cond [(and refs-info (hash-ref refs-info "release" #f))
            => (lambda (release-commit)
                 (define source* (make-git-source user repo path "release"))
                 (unless (equal? old-branch "release")
                   (eprintf "! Release branch discovered for ~a/~a~a\n"
                            user repo (if path (format "?path=~a" path) "")))
                 (unless (equal? release-commit old-checksum)
                   (eprintf "! Checksum updated for ~a\n" source*))
                 (hash-set! (checksum-updates) source*
                            (list old-checksum release-commit))
                 (hash-set* info 'source source* 'checksum release-commit))]
           [else #f])]
    [_ #f]))

(define (poll-source user repo cache)
  (hash-ref! cache (list user repo)
             (lambda ()
               (if (and (only-repos) (not (member (format "~a/~a" user repo) (only-repos))))
                   #f
                   (poll-source* user repo)))))

(define (poll-source* user repo)
  (for*/hash ([kind '("branches" "tags")]
              [ref-info (poll-source** user repo kind)])
    (values (hash-ref ref-info 'name)
            (hash-ref (hash-ref ref-info 'commit) 'sha))))

(define (poll-source** user repo kind)
  (define ref-infos
    (get/github (format "https://api.github.com/repos/~a/~a/~a" user repo kind)))
  (unless (and (list? ref-infos)
               (for/and ([ref-info ref-infos])
                 (and (hash? ref-info)
                      (hash-has-key? ref-info 'name)
                      (hash-has-key? ref-info 'commit))))
    (error 'poll-source "bad response from Github: ~e" ref-infos))
  ref-infos)

;; ------------------------------------------------------------

(define (command:poll args)
  (define *force? #f)
  (define *src-dir (current-directory))
  (define *dest-dir #f)
  (command-line
   #:argv args
   #:once-each
   [("-f" "--force") "Overwrite contents of dest-dir" (set! *force? #t)]
   [("-i" "--in") src-dir "Read catalog from src-dir" (set! *src-dir src-dir)]
   [("-o" "--out") dest-dir "Write catalog to dest-dir" (set! *dest-dir dest-dir)]
   [("--only") repo "Only poll given repo" (only-repos (list repo))])
  (set! *dest-dir (normalize-dest *src-dir *dest-dir *force?))
  (copy-catalog* *src-dir *dest-dir *force?
                 (lambda (catalog) (poll-catalog catalog))
                 (lambda (updates)
                   (unless (zero? (hash-count updates))
                     (call/write-log *dest-dir "poll-catalog"
                                     (lambda ()
                                       (pretty-write updates)))))))

(module+ main
  (command:poll (current-command-line-arguments)))
