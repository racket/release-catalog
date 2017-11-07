#!/bin/bash

# Finalizes the release catalog for a release

set -e  # abort on statement error
set -u  # abort on undefined variable
set -o pipefail # abort when any command in a pipe fails

RKTNVER=$1

# Get to top-level directory of release catalog
pushd `dirname $0` > /dev/null
cd ..

git pull

# Convert release branches to tags and change the catalog to point to the tags.
racket scripts/poll.rkt -fi release-catalog
racket scripts/add-tags.rkt -i release-catalog --tag v$RKTNVER
racket scripts/redirect.rkt -fi release-catalog \
       --from-branch release --branch v$RKTNVER
git commit -o . -m "final release catalog for v$RKTNVER"
git push

popd > /dev/null
