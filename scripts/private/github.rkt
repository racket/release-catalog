#lang racket/base
(require racket/file
         racket/port
         racket/match
         racket/hash
         racket/list
         racket/set
         racket/contract
         net/url
         net/base64
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
         ;; I think we should get rid of this one...
         get-commits-since-last-release
         results-per-page
         (contract-out
          [get-repo-commits-since-last-release
           (-> string? string? string? string? (listof (hash/c symbol? any/c)))])
         )



;; ============================================================
;; Github

(define USER-AGENT (format "User-Agent: racket-catalog-tool/~a" (version)))

;; wrap a generic non-data-accepting getter with a default "who"
;; and credentials added to the URL
(define (wrap/no-data who0 proc)
  (lambda (url #:headers [headers null]
               #:handle [handle (λ (a b) (read-json b))]
          #:fail [fail "failed"] #:who [who who0]
          #:credential-style [credential-style 'client])
    (proc url
          #:headers (list* USER-AGENT (credentials credential-style) headers)
          #:handle handle #:fail fail #:who who)))

;; wrap a generic data-accepting getter with a default "who"
;; and credentials added to the URL
(define (wrap/data who0 proc)
  (lambda (url #:headers [headers null]
               #:handle [handle (λ (a b) (read-json b))]
          #:fail [fail "failed"] #:who [who who0]
          #:credential-style [credential-style 'client]
          #:data [data #f])
    (proc url
          #:headers (list* USER-AGENT (credentials credential-style) headers)
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

(define (credentials credential-style)
  (cond
    [(equal? credential-style 'user)
     (cond [github-user-credentials
            (format "Authorization: token ~a" github-user-credentials)]
           [else
            (error 'add-credentials "user credentials required but not available")])]
    [(equal? credential-style 'client)
     ;; this appears to work. It's build according to RFC 2617
     (basic-auth-line (github-client_id) (github-client_secret))]
    [else
     (raise-argument-error 'add-credentials
                           "'client or 'user"
                           1 url credential-style)]))

;; given userid and password, return the authorization header line as described
;; by RFC 2617
(define (basic-auth-line userid password)
  (format "Authorization: Basic ~a"
          (base64-encode
           (string->bytes/utf-8 (format "~a:~a" userid password))
           #"")))

;; example from RFC 2617:
(module+ test
  (require rackunit)
  (check-equal? (basic-auth-line "Aladdin" "open sesame")
                "Authorization: Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ=="))

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

;; do a sequence of table lookups
(define (hash-chain-ref table keys)
  (for/fold ([table table])
            ([key (in-list keys)])
    (hash-ref table key)))

;; the structure of the commits returned by github appears to be essentially
;; undocumented, or maybe I'm just looking in the wrong place. Either way, if
;; it changes, you may have to change the next few functions. Hopefully
;; that's the only place you'll need to change.

;; a github commit appears to be a json object containing a number of fields,
;; including "author", "sha", "parents", and "commit". Unfortunately, the "author" field
;; doesn't actually include the author's name; that appears in the "author"
;; field of the "commit" field. Ugh.

;; return the sha of a commit. Well, the outer commit, anyway.
(define (commit-sha commit)
  (hash-ref commit 'sha))

;; return the sha's of the parents of the commit
(define (commit-parents commit)
  (for/list ([parent (in-list (hash-ref commit 'parents))])
    (hash-ref parent 'sha)))

;; return the textual name of the author
(define (commit-name commit)
  (hash-chain-ref commit '(commit author name)))

;; why is there no set-partition function?
(define (set-partition pred the-set)
  (for/fold ([yes (set)]
             [no (set)])
            ([val (in-set the-set)])
    (cond [(pred val) (values (set-add yes val) no)]
          [else (values yes (set-add no val))])))

(module+ test
  (define-values (evens odds)
    (set-partition even? (set 3 24 -242 33 9 12)))

  (check-equal? evens (set 24 -242 12))
  (check-equal? odds (set 3 33 9)))


(define results-per-page 250) ; github API says so

;; given a set of "start" shals and a hash mapping shas to commits,
;; find all nodes reachable through the "parent"
;; links, stopping whenever reaching something in the
;; "stop" set of shas or one that's already been found. Return
;; those found and a set of unfollowable links (because
;; they point outside the hash)
(define (reachable ids hash found stops unknown)
  (define non-stopped-ids (set-subtract ids (set-union stops found)))
  (cond [(set-empty? non-stopped-ids)
         (values found unknown)]
        [else
         (define-values (new-found not-found)
           (set-partition
            (λ (sha) (hash-has-key? hash sha))
            non-stopped-ids))
         (define new-found-commits
           (set-map new-found (λ (sha) (hash-ref hash sha))))
         (define new-reachable
           (list->set
            (apply append
                   (map commit-parents (set->list new-found-commits)))))
         (define new-unknown (set-union unknown not-found))
         (reachable new-reachable hash (set-union found new-found) stops new-unknown)]))

;; does the given tag exist for a repository?
(define (tag-exists? user repo tag)
  (define tags-fetched 100)
  (define url-string
    (url->string
     (url "https" #f "api.github.com" #f #t
          (map (λ (elt) (path/param elt '()))
               (list "repos" user repo "tags"))
          `((sha . ,tag)
            (per_page . ,(number->string tags-fetched))
            (page . "1"))
          #f)))
  (define get2-result
    (get/github
     url-string
     #:handle (λ (hdr body) (cons hdr (read-json body)))
     #:fail (lambda _
              (eprintf "! failed to get commits for ~a/~a\n"
                       user repo)
              (error 'get-commits-since-last-release
                     "failure to fetch url ~e\n"
                     url-string))))
  (define results (cdr get2-result))
  (define tag-names
    (map (λ (r) (hash-ref r 'name)) results))
  (when (<= tags-fetched (length tag-names))
    (error 'tag-exists?
           "fetched at least ~a tags, violating internal assumption. Increase tags-fetched?"
           tags-fetched))
  (and (member tag tag-names) #t))

;; get a page of commits, starting with the given sha
(define (fetch-commits user repo sha results-per-page page)
  (define url-string
    (url->string
     (url "https" #f "api.github.com" #f #t
          (map (λ (elt) (path/param elt '()))
               (list "repos" user repo "commits"))
          ;; always the same "start" sha
          `((sha . ,sha)
            (per_page . ,(number->string results-per-page))
            (page . ,(number->string page)))
          #f)))
  (define get2-result
    (get/github
     url-string
     #:handle (λ (hdr body) (cons hdr (read-json body)))
     #:fail (lambda _
              (eprintf "! failed to get commits for ~a/~a\n"
                       user repo)
              (error 'get-commits-since-last-release
                     "failure to fetch url ~e\n"
                     url-string))))
  ;; should validate Link header, instead of just assuming
  ;; that we can crank up the page counter.
  #;((define header (car get2-result))
     (let ([ans header])
       (eprintf "new-style headers: ~s\n" ans)
       ans)
     (eprintf "Link: ~s\n" (extract-field "Link" header))
     (eprintf "length foo : ~v\n" (length (cdr get2-result))))
  (define new-commits (cdr get2-result))
  (eprintf "# of new commits fetched: ~v\n" (length new-commits))
  new-commits)

(define (commits->table commits)
  (for/hash ([commit (in-list commits)])
    (values (commit-sha commit) commit)))

;; get-commits-since-last-release : Catalog Tag -> [Hashof user+repo [Listof commit]]
;;   where commit is in github's json format
;; Returns a hash table mapping the repos in `catalog` to the list of the
;; commits that appear in them since `tag`.
(define (get-commits-since-last-release catalog since-tag)
  (define sources (get-sources catalog))
  (for/hash ([(user+repo checksum) (in-hash (get-sources catalog))])
    (values user+repo (get-repo-commits-since-last-release user+repo checksum since-tag))))

;; given a github user, a repo associated with that user, the commit of a
;; candidate release, and a tag representing the prior release, return
;; a list of all of the commits which are ancestors of the given candidate
;; commit but are not ancestors of the prior release tag.
(define (get-repo-commits-since-last-release user repo release-commit since-tag)
  (eprintf "user+repo: ~v\n" (list user repo))
  (cond
    [(tag-exists? user repo since-tag)
          
     (define old-tag-sha
       (match (fetch-commits user repo since-tag 1 1)
         [(list commit) (commit-sha commit)]
         [other (error 'get-commits-since-last-release
                       "expected list of length 1 from fetch-commits, got: ~e"
                       other)]))
     ;; this is a messy problem. We're essentially finding the merge base,
     ;; and identifying all commits that are ancestors of the release branch
     ;; that aren't ancestors of the old-tag. The problem is that we have
     ;; no idea how big each set is. What we do is gradually expand our pool by
     ;; adding another page from each query (reachable-from-tag, reachable-from-release).
     ;; Each time we expand the pool, we start the search over again (because searching
     ;; should be more or less instant compared to fetching. So, for a given pool,
     ;; we first identify all ancestor commits reachable from the old-tag, ignoring loose ends
     ;; that point outside our set. These are our stops.
     ;; Then, we identify all commits reachable from the
     ;; release branch, halting the search on every path that reaches the stop pool.
     ;; if this search has loose ends, it means that we needed more commits in the
     ;; hash, so we fetch another page on each query and start over again.
     (cond
       ;; no fetch necessary for this short-cut case:
       [(equal? old-tag-sha release-commit) '()]
       [else
        (define unreleased-commits
            
          (let loop ([page 1] ;; in theory we should be following "Link" headers...
                     [commits-table (hash)])
            (define old-tag-commit-batch
              (fetch-commits user repo since-tag results-per-page page))
            (define new-tag-commit-batch
              (fetch-commits user repo release-commit results-per-page page))
            (define new-commits-table
              (hash-union commits-table
                          (commits->table old-tag-commit-batch)
                          (commits->table new-tag-commit-batch)
                          #:combine/key
                          (λ (k v0 v)
                            (cond [(equal? v0 v) v0]
                                  [else (error 'get-commits-since-last-release
                                               "key ~v has maps to two different values: ~e and ~e"
                                               k v0 v)]))))
            (define-values (stops _)
              (reachable (set old-tag-sha) new-commits-table (set) (set) (set)))
            (define-values (unreleased-shas loose-ends)
              (reachable (set release-commit) new-commits-table (set) stops (set)))
            (cond [(set-empty? loose-ends)
                   (set-map unreleased-shas (λ (sha) (hash-ref new-commits-table sha)))]
                  [else
                   ;; try again with another page on each query...
                   (loop (add1 page) new-commits-table)])))
        (set->list unreleased-commits)])]
    [else
     (eprintf "! repo ~a/~a does not contain a tag with name ~a. Assuming It's a new repo\n"
              user repo since-tag)
     ;; just keep going until you get zero? This seems sketchy.
     (let loop ([page 1])
       (define commits
         (fetch-commits user repo release-commit results-per-page page))
       (cond [(empty? commits)
              '()]
             [else (append commits (loop (add1 page)))]))]))

(module+ test
  (check-not-exn
   (λ () (get/github "https://api.github.com/repos/racket/db/branches")))
  (check-equal? (tag-exists? "racket" "racket" "v8.3") #t))