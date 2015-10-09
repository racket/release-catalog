#lang racket/base
(require racket/cmdline
         racket/match
         racket/pretty
         "private/util.rkt"
         "private/github.rkt")
(provide (all-defined-out))

;; For each pkg in the catalog matching <from-user>/<from-repo>#<from-branch>,
;; check for the current value of #<from-branch> and update the pkg's
;; checksum field.

;; Adapted from pkg-index/official/update.rkt

;; checksum-updates : hash[ source  => (list old-checksum new-checksum) ]
(define checksum-updates (make-parameter (make-hash)))

;; update-checksums : Catalog -> (values Catalog Hash)
(define (update-checksums catalog
                          #:from-user [from-user #f]
                          #:from-repo [from-repo #f]
                          #:from-branch [from-branch #f])
  (define update? (make-update-pkg? from-user from-repo from-branch))
  (parameterize ((checksum-updates (make-hash)))
    (define new-catalog
      (for/hash ([(name info) (in-hash catalog)])
        (check-minimized name info)
        (values name
                (if (update? info)
                    (update-checksum name info)
                    info))))
    (values new-catalog
            (for/hash ([(source old+new) (in-hash (checksum-updates))]
                       #:when (not (equal? (car old+new) (cadr old+new))))
              (values source old+new)))))

;; make-update-pkg? : String/#f String/#f String/#f -> InfoHash -> Boolean
(define ((make-update-pkg? from-user from-repo from-branch) info)
  (define source (hash-ref info 'source))
  (define (skip [why ""])
    ;; (eprintf "! Not updating~a ~s\n" why source)
    #f)
  (match (parse-repo-url source)
    [(list user repo branch path)
     (cond [(and from-user (not (equal? user from-user)))
            (skip " (user doesn't match)")]
           [(and from-repo (not (equal? repo from-repo)))
            (skip " (repo doesn't match)")]
           [(and from-branch (not (equal? (or branch "master") from-branch)))
            (skip " (branch doesn't match)")]
           [else #t])]
    ['native
     ;; Skip silently
     #f]
    [#f
     (skip " (not a github repo)")]))

;; update-checksum : Any InfoHash -> InfoHash
(define (update-checksum name info0)
  (define info (hash-copy info0))
  (define source (hash-ref info 'source))
  (define old-checksum (hash-ref info 'checksum))
  (define new-checksum
    (cond [(hash-ref (checksum-updates) source #f)
           => (lambda (old+new) (cadr old+new))]
          [else
           ;; (eprintf "! Fetching checksum for ~s\n" source)
           (define new-checksum
             (package-url->checksum source #:pkg-name name))
           (hash-set! (checksum-updates) source (list old-checksum new-checksum))
           new-checksum]))
  (hash-set! info 'checksum new-checksum)
  (unless (equal? new-checksum old-checksum)
    (eprintf "! Checksum updated for ~s\n" name))
  info)

;; ------------------------------------------------------------

(define (command:checksums args)
  (define *force? #f)
  (define *src-dir (current-directory))
  (define *dest-dir #f)
  (define *from-user #f)
  (define *from-repo #f)
  (define *from-branch #f)
  (command-line
   #:argv args
   #:once-each
   [("-f" "--force") "Overwrite contents of dest-dir" (set! *force? #t)]
   [("-i" "--in") src-dir "Read catalog from src-dir" (set! *src-dir src-dir)]
   [("-o" "--out") dest-dir "Write catalog to dest-dir" (set! *dest-dir dest-dir)]
   [("--from-user") from-user "Only packages with given user" (set! *from-user from-user)]
   [("--from-repo") from-repo "Only packages with given repo name" (set! *from-repo from-repo)]
   [("--from-branch") from-branch "Only packages on given branch" (set! *from-branch from-branch)])
  (set! *dest-dir (normalize-dest *src-dir *dest-dir *force?))
  (copy-catalog* *src-dir *dest-dir *force?
                 (lambda (catalog) (update-checksums catalog
                                                #:from-user *from-user
                                                #:from-repo *from-repo
                                                #:from-branch *from-branch))
                 (lambda (updates)
                   (unless (zero? (hash-count updates))
                     (call/write-log *dest-dir "checksum-updates"
                                     (lambda () (pretty-write updates)))))))

(module+ main
  (command:checksums (current-command-line-arguments)))
