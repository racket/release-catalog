#lang racket/base
(require racket/file
         racket/port
         racket/match
         net/url
         json
         (only-in pkg/util ;; pkg/private/stage on master
                  package-url->checksum
                  github-client_id
                  github-client_secret))
(provide (all-defined-out))

;; ============================================================
;; Github

(define (github-api u #:query [query-parts null])
  (define api-url (build-url u #:query (append (github-credentials) query-parts)))
  (define result
    (call/input-url+200
     api-url port->bytes
     #:headers (list (format "User-Agent: raco-pkg/~a" (version)))))
  (unless result
    (error 'github-api "could not connect to github ~s" (url->string api-url)))
  (read-json (open-input-bytes result)))

(define (github-credentials)
  (cond [(and (github-client_id)
              (github-client_secret))
         (list (cons 'client_id (github-client_id))
               (cons 'client_secret (github-client_secret)))]
        [else null]))

;; ----------------------------------------

(let ([credentials-file
       (build-path (find-system-path 'pref-dir) "github-poll-client.rktd")])
  (cond [(file-exists? credentials-file)
         (define credentials (file->value credentials-file))
         (github-client_id (car credentials))
         (github-client_secret (cadr credentials))]
        [else
         (eprintf "! No github credentials found.\n")]))

;; ----------------------------------------

(define (call/input-url+200 u fun
                            #:headers [headers '()]
                            #:failure [fail-k (lambda (s) #f)])
  (define-values (ip hs)
    (get-pure-port/headers u headers
                           #:redirections 25
                           #:status? #t))
  (if (string=? "200" (substring hs 9 12))
      (begin0 (fun ip)
        (close-input-port ip))
      (fail-k hs)))

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
