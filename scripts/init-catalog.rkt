#lang racket

;; initialize the catalog for a release. Replaces init-catalog.sh

(require racket/runtime-path
         pkg/lib
         "filter.rkt"
         "private/util.rkt"
         "checksums.rkt")

(define version
  (command-line
   #:args (version)
   version))


(define (system+ str)
  (printf "cmd: ~v\n" str)
  (system str))

(define release-catalog-path "release-catalog")

(define-runtime-path release-catalog-dir "..")

(module+ main
  (current-directory release-catalog-dir)

  (system+ "git pull")

  (when (directory-exists? "release-catalog")
    (system+ "git rm -rf release-catalog")
    (system+ "git commit -o . -m \"delete old catalog\""))

  (when (directory-exists? "release-catalog")
    (error "git rm did not remove directory, clean it up manually"))

  ;; or maybe the real problem is that raco pkg catalog-copy now includes
  ;; an error check?

  (printf "copy catalog...\n")
  (pkg-catalog-copy '("https://pkgs.racket-lang.org")
                    ;; NB dependency on current directory:
                    release-catalog-path)
  (printf "filtering catalog...\n")
  (let ()
    (define *force? #t)
    ;; NB current directory dependency
    (define *src-dir release-catalog-path)
    (define *roots '("main-distribution" "main-distribution-test" "distro-build"))
    (copy-catalog *src-dir *src-dir *force?
                  (λ (catalog) (filter-catalog catalog *roots))))
  (printf "updating checksums...\n")
  (let ()
    (define *force? #t)
    (define *src-dir release-catalog-path)
    (define *dest-dir *src-dir)
    (copy-catalog* *src-dir *dest-dir *force?
                   (lambda (catalog) (update-checksums catalog
                                                       #:from-user #f
                                                       #:from-repo #f
                                                       #:from-branch #f))
                   (lambda (updates)
                     (unless (zero? (hash-count updates))
                       (call/write-log *dest-dir "checksum-updates"
                                       (lambda () (pretty-write updates)))))))

  ;; gross, use system* instead...
  (system+ "git add release-catalog")
  (system+ (~a "git commit -o . -m \"begin release catalog for "version"\""))
  (system+ "git push")

  )

#|
# Get to top-level directory of release catalog
pushd `dirname $0` > /dev/null
cd ..

git pull

## BUG here: if there are leftover files in release-catalog, e.g.
## logs/branch-redirected-files, then the script will fail on the 
## raco pkg catalog-copy. fix this.
# Delete old catalog, if it exists
git rm -rf release-catalog
git commit -o . -m "delete old catalog"

# Get the current catalog, remove irrelevant pkgs, get latest checksums
raco pkg catalog-copy https://pkgs.racket-lang.org release-catalog
racket scripts/filter.rkt -fi release-catalog \
       main-distribution main-distribution-test distro-build
racket scripts/checksums.rkt -fi release-catalog

# Commit and push
git add release-catalog
git commit -o . -m "begin release catalog for v$RKTNVER"
git push

popd > /dev/null
|#
