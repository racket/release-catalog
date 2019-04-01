#lang racket/base
(require racket/file
         racket/port
         racket/match
         net/url
         json
         "net.rkt"
         "util.rkt"
         (only-in pkg/private/stage
                  package-url->checksum
                  github-client_id
                  github-client_secret))
(provide package-url->checksum
         USER-AGENT
         get/github
         head/github
         delete/github
         post/github
         put/github
         build-url
         get-commits-since-last-release
         results-per-page
         )

;; ============================================================
;; Github

(define USER-AGENT (format "User-Agent: racket-catalog-tool/~a" (version)))

;; wrap a generic non-data-accepting getter with a default "who"
;; and credentials added to the URL
(define (wrap/no-data who0 proc)
  (lambda (url #:headers [headers null] #:handle [handle read-json]
          #:fail [fail "failed"] #:who [who who0]
          #:credential-style [credential-style 'client])
    (proc (add-credentials url credential-style)
          #:headers (cons USER-AGENT headers)
          #:handle handle #:fail fail #:who who)))

;; wrap a generic data-accepting getter with a default "who"
;; and credentials added to the URL
(define (wrap/data who0 proc)
  (lambda (url #:headers [headers null] #:handle [handle read-json]
          #:fail [fail "failed"] #:who [who who0]
          #:credential-style [credential-style 'client]
          #:data [data #f])
    (proc (add-credentials url credential-style)
          #:headers (cons USER-AGENT headers)
          #:handle handle #:fail fail #:who who #:data data)))

(define get/github (wrap/no-data 'get/github get/url))
(define head/github (wrap/no-data 'head/github head/url))
(define delete/github (wrap/no-data 'delete/github delete/url))

(define post/github (wrap/data 'post/github post/url))
(define put/github (wrap/data 'put/github put/url))

;; ----------------------------------------

;; given a url, add query terms representing either user credentials
;; (if user-credentials? is 'user) or client credentials
(define (add-credentials url credential-style)
  (cond
    [(equal? credential-style 'user)
     (cond [github-user-credentials
            (url-add-query url (list (cons 'access_token github-user-credentials)))]
           [else
            (error 'add-credentials "user credentials required but not available")])]
    [(equal? credential-style 'client)
     (url-add-query url (github-client-credentials))]
    [else
     (raise-argument-error 'add-credentials
                           "'client or 'user"
                           1 url credential-style)]))

;; a personal access token, obtained from github>settings>developer settings
(define github-user-credentials
  (let ([file (build-path (find-system-path 'pref-dir) "github-user-credentials.rktd")])
    (and (file-exists? file)
         (file->value file))))

;; set the client & secret tokens
(let ([credentials-file
       (build-path (find-system-path 'pref-dir) "github-poll-client.rktd")])
  (cond [(file-exists? credentials-file)
         (define credentials (file->value credentials-file))
         ;; two parameters associated with the Github "Web App" protocol
         (github-client_id (car credentials))
         (github-client_secret (cadr credentials))]
        [else
         (eprintf "! No github credentials for polling client App found.\n")]))

;; return the client credentials
(define (github-client-credentials)
  (cond [(and (github-client_id)
              (github-client_secret))
         (list (cons 'client_id (github-client_id))
               (cons 'client_secret (github-client_secret)))]
        [else null]))

;; ----------------------------------------

(define (build-url base
                   #:path [path-parts null]
                   #:query [query-parts null]
                   #:fragment [fragment #f])
  (build-url* base path-parts query-parts fragment))

(define (build-url* base path-parts query-parts fragment)
  (match base
    [(? string)
     (build-url* (string->url base) path-parts query-parts fragment)]
    [(url scheme user host port path-absolute? path query old-fragment)
     (define path* (append path (map build-path/param path-parts)))
     (define query* (append query query-parts))
     (define fragment* (or fragment old-fragment))
     (url scheme user host port path-absolute? path query fragment*)]))

(define (build-path/param p)
  (cond [(path/param? p) p]
        [else (path/param p null)]))

;; ----------------------------------------


(define results-per-page 250) ; github API says so

;; get-commits-since-last-release : Catalog Tag -> [Hashof repo [Listof commit]]
;;   where commit is in github's json format
;; Returns a hash table mapping the repos in `catalog` to the list of the
;; commits that appear in them since `tag`.
(define (get-commits-since-last-release catalog since-tag)
  (for/hash ([(user+repo checksum) (in-hash (get-sources catalog))])
    (match user+repo
      [(list user repo)
       (define-values (commits-since-last-release _1 _2)
         (for/fold ([commits '()]
                    [end   checksum]
                    [done?   #f])
             ([page (in-naturals 1)]
              #:break done?)
           (define res
             (hash-ref
              (get/github
               (format "https://api.github.com/repos/~a/~a/compare/~a...~a?per_page=~a;page=~a"
                       user repo since-tag end results-per-page page)
               #:handle read-json
               #:fail (lambda _
                        (eprintf "! failed to get commits for ~a/~a\n"
                                 user repo)
                        ;; safe to ignore and keep going
                        (hash 'commits '())))
              'commits))
           (define d? (not (= (length res) results-per-page)))
           (values (append res commits)
                   (and (not d?)
                        (hash-ref (car res) 'sha)) ; rest of range
                   d?)))
       (values repo commits-since-last-release)])))
