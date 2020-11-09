#lang racket/base
(require racket/file
         racket/path
         racket/date
         racket/match
         racket/contract)
(provide
 (contract-out [get-sources (-> any/c
                                (hash/c any/c string?))])
 (except-out (all-defined-out) get-sources))

;; ============================================================
;; Utilities

;; ----------------------------------------
;; For catalog entries

;; An InfoHash is Hash[ Symbol => Any ]

;; minimized-info : InfoHash
(define (minimize-info src)
  (define fields '(source checksum name author authors description dependencies))
  (define dest (make-hash))
  (hash-copy-fields! dest src fields)
  (define versions (hash-ref src 'versions #hash()))
  (define default (hash-ref versions 'default #hash()))
  (hash-copy-fields! dest default fields)
  dest)

;; check-minimized : Any InfoHash -> Void
;; Raise error if info hash has a 'versions key (removed by minimize-info).
(define (check-minimized name info)
  (when (hash-has-key? info 'versions)
    (error myself-sym "catalog info not minimized for package: ~s" name)))

;; hash-copy-fields : MutableHash Hash (Listof Symbol) => Void
;; Copy given fields from src to dest; silently skip non-existing fields.
(define (hash-copy-fields! dest src fields)
  (for ([field fields])
    (when (hash-has-key? src field)
      (hash-set! dest field (hash-ref src field)))))

;; ============================================================

;; FIXME: the repos to create a release branch on are logged as a side effect of
;; the branch subcommand. TODO: make it easier to extract them from the log?

;; FIXME: the repos with updated checksums are logged as a side effect
;; of the checksums command. TODO: use them to generate a change
;; report to email out?

;; ============================================================
;; Repo URLs (Composing and Parsing)

;; make-git-source : String String String String -> String
;; Compose a git source URL from parts.
(define (make-git-source user repo path to-branch #:url-type [url-type 'git])
  (define branch-part
    (cond [(member to-branch '(#f "master")) ""]
          [else (format "#~a" to-branch)]))
  (case url-type
    [(git)
     (format "git://github.com/~a/~a~a~a"
             user repo (if path (format "/?path=~a" path) "") branch-part)]
    [(https)
     (format "https://github.com/~a/~a.git~a~a"
             user repo (if path (format "/?path=~a" path) "") branch-part)]))

;; A ParsedRepo is one of
;; - (list user repo branch path)
;;   where user, repo : String; and branch, path : (U String #f)
;;   represents github source
;; - 'native
;;   represents native zip pkg
;; - #f
;;   represents source in unknown format (couldn't parse)

;; parse-repo-url : String -> ParsedRepo
(define (parse-repo-url url)
  (cond [(for/or ([parser github-source-parsers]) (parser url))
         => values]
        [(regexp-match? native-pkg-source-rx url)
         'native]
        [else #f]))

(define (rx->parser permute . rxs)
  (define rx (regexp (apply string-append (append '("^") rxs '("$")))))
  (lambda (url)
    (cond [(regexp-match rx url)
           => (lambda (m) (for/list ([index permute]) (list-ref m index)))]
          [else #f])))

(define path-part-rx
  "(?:[?]path=([^?#]*))?")

;; produce (list user repo branch path)
(define github-source-parsers
  (list (rx->parser '(1 2 4 3)
                    "git://github\\.com/([^?/#]+)/([^/?#]+)/?"
                    path-part-rx
                    "(?:#([^/?#]+))?")
        (rx->parser '(1 2 3 4)
                    "github://github\\.com/([^?/#]+)/([^/?#]+)/([^/?#]+)/?" ;; optimization coach
                    path-part-rx)
        (rx->parser '(1 2 3 4)
                    "http://github\\.com/([^?/#]+)/([^/?#]+)/tree/([^/?#]+)/?"
                    path-part-rx)))

(define native-pkg-source-rx #rx"^http://racket-packages\\.s3") ;; currently unused


(parse-repo-url "https://github.com/Metaxal/quickscript.git")


;; ============================================================
;; Catalog Reading and Writing
;; Adapted from pkg/private/catalog-copy.rkt

(define (read-catalog src-dir)
  (define catalog-file (build-path src-dir "pkgs-all"))
  (unless (file-exists? catalog-file)
    (raise-user-error myself-sym
                      "catalog file does not exist: ~s" catalog-file))
  (with-input-from-file catalog-file read))

(define (write-catalog dest-path details
                       #:force? [force? #f])
  ;; Note: preserve logs subdirectory!
  (when force?
    (cond [(file-exists? dest-path)
           (delete-file dest-path)]
          [(directory-exists? dest-path)
           (for ([i (directory-list dest-path)]
                 #:when (not (equal? i (build-path "logs"))))
             (delete-directory/files (build-path dest-path i)))]
          [(link-exists? dest-path)
           (delete-file dest-path)]))
  (define pkg-path (build-path dest-path "pkg"))
  (make-directory* pkg-path)
  (for ([(k v) (in-hash details)])
    (call-with-output-file*
     #:exists 'truncate/replace
     (build-path pkg-path k)
     (lambda (o) (write-ordered v o))))
  (call-with-output-file*
   #:exists 'truncate/replace
   (build-path dest-path "pkgs")
   (lambda (o) (write (sort (hash-keys details) string<?) o)))
  (call-with-output-file*
   #:exists 'truncate/replace
   (build-path dest-path "pkgs-all")
   (lambda (o) (write-ordered details o))))

(define (write-ordered v o)
  (cond [(hash? v)
         (cond [(hash-eq? v)
                (fprintf o "\n#hasheq(")]
               [else
                (fprintf o "\n#hash(")])
         (for ([k (sort (hash-keys v) key<?)])
           (define vk (hash-ref v k))
           (fprintf o "\n(~s . " k)
           (write-ordered vk o)
           (fprintf o ")"))
         (fprintf o ")")]
        [else
         (write v o)]))

(define (key<? a b)
  (cond [(and (string? a) (string? b))
         (string<? a b)]
        [(and (symbol? a) (symbol? b))
         (symbol<? a b)]
        [else (string? a)]))

(define (call/write-log dest-path log-file proc)
  (make-directory* (build-path dest-path "logs"))
  (define now (current-seconds))
  (define out (open-output-string))
  (begin0 (parameterize ((current-output-port out)) (proc))
    (maybe-write-log (build-path dest-path "logs" log-file)
                     now
                     (get-output-string out))))

(define (maybe-write-log log-file now output)
  (unless (zero? (string-length output))
    (with-output-to-file log-file
      #:exists 'append
      (lambda ()
        (printf ";; ~a\n" (make-string 60 #\=))
        (printf ";; Appended ~s : ~a\n" now (date->string (seconds->date now) #t))
        (write-string output)))))

(define (copy-catalog src dest force? transform)
  (copy-catalog* src dest force? (lambda (c) (values (transform c) (void)))))

(define (copy-catalog* src dest force? transform [handle-other void])
  (define catalog (read-catalog src))
  (define-values (new-catalog other) (transform catalog))
  (write-catalog dest new-catalog #:force? force?)
  (handle-other other))

(define (normalize-dest src dest force?)
  (or dest
      (and force? src)
      (raise-user-error myself-sym "destination not given and force? not set")))

;; get-sources : Catalog -> Hash[ (List String String) => String ]
(define (get-sources catalog)
  ;; repos : Hash[ (List String String) => String ]
  (define repos (make-hash))
  (for ([(pkg-name info) (in-hash catalog)])
    (define src (hash-ref info 'source))
    (match (parse-repo-url src)
      [(list user repo branch path)
       ;; Assume checksums consistent for all srcs w/ user/repo.
       (hash-set! repos (list user repo) (hash-ref info 'checksum))]
      [_ (void)]))
  repos)

;; ============================================================
;; Commands

(define myself (path->string (file-name-from-path (find-system-path 'run-file))))
(define myself-sym (string->symbol myself))
(define (subcommand sub) (format "~a ~a" myself sub))
