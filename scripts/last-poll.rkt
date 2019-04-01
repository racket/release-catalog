#lang racket/base
(require racket/match
         racket/pretty
         racket/cmdline
         racket/hash
         "private/util.rkt")
(provide (all-defined-out))

;; Show results of last run of poll.rkt, condensing multiple pkgs with
;; same repo.

;; ============================================================
;; Show results of last run of poll.rkt

;; A PollLog is (Hashof String => (cons String String))

;; condense-log : PollLog -> (Listof (list* String String String))
(define (condense-log poll-log)
  ;; h : Hashof[ (list* String String String) => #t ]
  (define h (make-hash))
  (for ([(source0 old+new) (in-hash poll-log)])
    (define source
      (match (parse-repo-url source0)
        [(list user repo path branch)
         (make-git-source user repo #f #f)]))
    (hash-set! h (cons source old+new) #t))
  (sort (hash-keys h) string<? #:key car))

(define (show-condensed-log entries)
  (for ([entry (in-list entries)])
    (match entry
      [(list source old new)
       (printf "~a\n" source)
       (printf "  ~a -> ~a\n" (trim old) (trim new))])))

(define (trim sha)
  (substring sha 0 10))

;; read-{last-log,all-logs} : -> PollLog
;; PRE: current-input-port is log file
(define (read-last-log)
  (for/last ([log (in-port)]) log))
(define (read-all-logs)
  (for/fold ([h (hash)]) ([log (in-port)]) (merge-logs h log)))

(define (merge-logs a b)
  (hash-union a b #:combine (lambda (av bv) (cons (car av) (cdr bv)))))

;; ------------------------------------------------------------

(define (command:last-poll args)
  (define *src-dir (current-directory))
  (define *all? #f)
  (command-line
   #:argv args
   #:once-each
   [("--all") "Read and merge all logs (not just last)" (set! *all? #t)]
   [("-f" "--force") "Does nothing" (void)]
   [("-i" "--in") src-dir "Read catalog from src-dir" (set! *src-dir src-dir)])
  (define last-log
    (or (with-input-from-file (build-path *src-dir "logs" "poll-catalog")
          (lambda () (if *all? (read-all-logs) (read-last-log))))
        (error 'last-poll "poll.rkt log is empty or missing")))
  (show-condensed-log
   (condense-log last-log)))

(module+ main
  (command:last-poll (current-command-line-arguments)))
