#!/bin/bash

# Initializes the release catalog for a release

set -e  # abort on statement error
set -u  # abort on undefined variable
set -o pipefail # abort when any command in a pipe fails

# Get to top-level directory of release catalog
pushd `dirname $0` > /dev/null
cd ..

git pull

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
