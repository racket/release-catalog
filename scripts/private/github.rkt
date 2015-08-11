#lang racket/base
(require racket/file
         racket/port
         racket/match
         net/url
         json
         "net.rkt"
         (only-in pkg/private/stage
                  package-url->checksum
                  github-client_id
                  github-client_secret))
(provide package-url->checksum
         (all-defined-out))

;; ============================================================
;; Github

(define USER-AGENT (format "User-Agent: racket-catalog-tool/~a" (version)))

(define (wrap/no-data who0 proc)
  (lambda (url #:headers [headers null] #:handle [handle read-json]
          #:fail [fail "failed"] #:who [who who0]
          #:user-credentials? [user-credentials? #f])
    (proc (add-credentials url user-credentials?)
          #:headers (cons USER-AGENT headers)
          #:handle handle #:fail fail #:who who)))

(define (wrap/data who0 proc)
  (lambda (url #:headers [headers null] #:handle [handle read-json]
          #:fail [fail "failed"] #:who [who who0]
          #:user-credentials? [user-credentials? #f]
          #:data [data #f])
    (proc (add-credentials url user-credentials?)
          #:headers (cons USER-AGENT headers)
          #:handle handle #:fail fail #:who who #:data data)))

(define get/github (wrap/no-data 'get/github get/url))
(define head/github (wrap/no-data 'head/github head/url))
(define delete/github (wrap/no-data 'delete/github delete/url))

(define post/github (wrap/data 'post/github post/url))
(define put/github (wrap/data 'put/github put/url))

;; ----------------------------------------

(define (add-credentials url user-credentials?)
  (cond [(and user-credentials? github-user-credentials)
         (url-add-query url (list (cons 'access_token github-user-credentials)))]
        [user-credentials?
         (error 'add-credentials "user credentials required but not available")]
        [else
         (url-add-query url (github-client-credentials))]))

(define github-user-credentials
  (let ([file (build-path (find-system-path 'pref-dir) "github-user-credentials.rktd")])
    (and (file-exists? file)
         (file->value file))))

(let ([credentials-file
       (build-path (find-system-path 'pref-dir) "github-poll-client.rktd")])
  (cond [(file-exists? credentials-file)
         (define credentials (file->value credentials-file))
         (github-client_id (car credentials))
         (github-client_secret (cadr credentials))]
        [else
         (eprintf "! No github credentials found.\n")]))

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
