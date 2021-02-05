#lang racket/base
(require racket/match
         openssl
         net/url
         net/url-connect)
(provide get/url
         head/url
         delete/url
         post/url
         put/url
         url-add-query
         form-headers)

;; Turn on verification on unix systems where ca-certificates.crt exists.
(define verifying-ssl-context (ssl-secure-client-context))

#|
TODO: add redirect option like get-pure-port
|#

(define (do-method who url method data?
                   handle fail headers data)
  (let* ([url (if (string? url) (string->url url) url)]
         [data (if (string? data) (string->bytes/utf-8 data) data)])
    (unless (equal? (url-scheme url) "https")
      (error who "insecure location (expected `https' scheme): ~e" (url->string url)))
    (call/input-url url
      (lambda (url)
        (parameterize ((current-https-protocol
                        (if (ssl-client-context? (current-https-protocol))
                            (current-https-protocol)
                            verifying-ssl-context)))
          (if data?
              (method url data headers)
              (method url headers))))
      (lambda (in)
        (let ([response-header (purify-port in)])
          (cond [(regexp-match? ok-rx response-header)
                 (handle response-header in)]
                [else
                 (if (string? fail)
                     (error who "~a\n  url: ~e\n  response: ~e" fail (url->string url)
                            (read-line (open-input-string response-header) 'any))
                     (fail response-header in))]))))))

(define ok-rx #rx"^HTTP/1\\.. 20.")

;; ----

(define (mk-no-data-method who0 method)
  (lambda (url
           #:headers [headers null]
           #:handle [handle void]
           #:who [who who0]
           #:fail [fail "failed"])
    (do-method who url method #f
               handle fail headers #f)))

(define get/url (mk-no-data-method 'get/url get-impure-port))
(define head/url (mk-no-data-method 'head/url head-impure-port))
(define delete/url (mk-no-data-method 'delete/url delete-impure-port))

(define (mk-data-method who0 method)
  (lambda (url
           #:headers [headers null]
           #:data [data #f]
           #:handle [handle void]
           #:who [who who0]
           #:fail [fail "failed"])
    (do-method who url method #t
               handle fail headers data)))

(define post/url (mk-data-method 'post/url post-impure-port))
(define put/url (mk-data-method 'put/url put-impure-port))

;; ----

;; url-add-query : string/url alist -> url
(define (url-add-query base-url query-alist)
  (match (if (string? base-url) (string->url base-url) base-url)
    [(url scheme user host port path-abs? path query fragment)
     (let ([query (append query query-alist)])
       (url scheme user host port path-abs? path query fragment))]))

(define (form-headers)
  '("Content-Type: application/x-www-form-urlencoded"))
