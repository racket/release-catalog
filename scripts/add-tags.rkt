#lang racket/base
(require racket/match
         racket/date
         racket/cmdline
         racket/port
         json
         "private/util.rkt"
         "private/github.rkt")
(provide (all-defined-out))

;; For each source in the catalog matching */*#<branch>, create an
;; annotated tag named <tag> pointing to the source's stored checksum
;; (which might be different than the current value of #<branch>).

#|
(define (my-name) "Ryan Culpepper")
(define (my-email) "ryanc@racket-lang.org")
(define (current-iso8601-timestamp)
  ;; FIXME: leaves of final "Z"; github rejects
  (parameterize ((date-display-format 'iso-8601))
    (date->string (seconds->date (current-seconds) #t) #t)))
|#

;; add-tags : Catalog String String -> Void
(define (add-tags catalog tag-name tag-message)
  (for ([(user+repo checksum) (in-hash (get-sources catalog))])
    (match user+repo
      [(list user repo)
       (add-tag user repo tag-name tag-message checksum)])))

;; add-tag : ... -> Void
(define (add-tag user repo tag-name tag-message commit-sha)
  (define already-exists? (tag-exists? user repo tag-name))
  (when already-exists?
    (eprintf "! Tag ~s already exists for ~a/~a\n" tag-name user repo))
  (unless already-exists?
    (define tag-result
      (post/github (format "https://api.github.com/repos/~a/~a/git/tags" user repo)
                   #:user-credentials? #t
                   #:fail (lambda (response-header in)
                            (eprintf "creating tag failed for ~a/~a, tag ~a:\n  ~a\n"
                                     user repo tag-name response-header)
                            (eprintf "Response:\n~a\n" (port->bytes in))
                            (error 'create-tag "failed"))
                   #:headers '("Content-Type: application/json")
                   #:data
                   (jsexpr->bytes
                    (hash 'tag tag-name
                          'message tag-message
                          'object commit-sha
                          'type "commit"
                          #|
                          'tagger
                          (hash 'name (my-name)
                                'email (my-email)
                                'date (current-iso8601-timestamp))
                          |#))))
    (define tag-sha (hash-ref tag-result 'sha))
    (define tag-ref-result
      (post/github (format "https://api.github.com/repos/~a/~a/git/refs" user repo)
                   #:user-credentials? #t
                   #:data
                   (jsexpr->bytes
                    (hash 'ref (format "refs/tags/~a" tag-name)
                          'sha tag-sha))))
    (eprintf "! Created tag ~s for ~a/~a\n" tag-name user repo)
    (void)))

;; tag-exists? : String String String -> Boolean
(define (tag-exists? user repo tag-name)
  (define result
    (get/github (format "https://api.github.com/repos/~a/~a/git/refs/tags/~a"
                        user repo tag-name)
                #:fail (lambda (response-header in)
                         '(eprintf "tag-exists? : failed for ~a/~a, tag ~a:\n  ~a\n"
                                   user repo tag-name response-header)
                         #f)))
  (and result))

;; ------------------------------------------------------------

(define (command:add-tags args)
  (define *src-dir (current-directory))
  (define *tag-name #f)
  (define *tag-message "")
  (command-line
   #:argv args
   #:once-each
   [("-f" "--force") "Does nothing" (void)]
   [("-i" "--in") src-dir "Read catalog from src-dir" (set! *src-dir src-dir)]
   [("--tag") tag-name "Name of tag to create" (set! *tag-name tag-name)]
   [("-m" "--message") tag-message "Message" (set! *tag-message tag-message)]
   [("--from-branch") from-branch "Does nothing (for backwards compatibility)" (void)])
  (unless *tag-name
    (error 'add-tags "tag name required"))
  (add-tags (read-catalog *src-dir) *tag-name *tag-message))

(module+ main
  (command:add-tags (current-command-line-arguments)))
