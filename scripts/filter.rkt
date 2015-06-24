#lang racket/base
(require racket/cmdline
         "private/util.rkt")
(provide (all-defined-out))

;; Filter the catalog, retaining only pkg entries reachable by
;; dependency edges from <roots>, and minimizing the pkg info hashes.

;; ============================================================
;; Filter catalog

;; Also minimizes catalog entries to eliminate multiple versions, etc.

(define (filter-catalog catalog root-pkgs)
  (eprintf "! Filtering catalog with roots: ~s\n" root-pkgs)
  (filter-catalog* catalog (make-hash) root-pkgs))

(define (filter-catalog* src dest pkgs)
  (let loop ([pkgs pkgs])
    (cond [(null? pkgs) (void)]
          [(hash-ref dest (car pkgs) #f)
           ;; (eprintf "! Skipping pkg already processed: ~s\n" (car pkgs))
           (loop (cdr pkgs))]
          [(hash-ref src (car pkgs) #f)
           => (lambda (info)
                ;; (eprintf "! Adding pkg: ~s\n" (car pkgs))
                (hash-set! dest (car pkgs) (minimize-info info))
                (define deps (hash-ref info 'dependencies))
                (loop (append (map dep->pkg-name deps) (cdr pkgs))))]
          [else
           (eprintf "! Skipping unknown package: ~s\n" (car pkgs))
           (loop (cdr pkgs))]))
  dest)

(define (dep->pkg-name dep)
  (cond [(string? dep) dep]
        [(pair? dep) (car dep)]
        [else (error 'dep->pkg-name "bad dependency: ~s" dep)]))

;; ------------------------------------------------------------

(define (command:filter args)
  (define *force? #f)
  (define *src-dir (current-directory))
  (define *dest-dir #f)
  (define *roots null)
  (command-line
   #:program (subcommand "filter")
   #:argv args
   #:once-each
   [("-f" "--force") "Overwrite contents of dest-dir" (set! *force? #t)]
   [("-i" "--in") src-dir "Read catalog from src-dir" (set! *src-dir src-dir)]
   [("-o" "--out") dest-dir "Write catalog to dest-dir" (set! *dest-dir dest-dir)]
   #:args root-pkgs
   (set! *roots root-pkgs))
  (set! *dest-dir (normalize-dest *src-dir *dest-dir *force?))
  (copy-catalog *src-dir *dest-dir *force?
                (lambda (catalog) (filter-catalog catalog *roots))))

(module+ main
  (command:filter (current-command-line-arguments)))
