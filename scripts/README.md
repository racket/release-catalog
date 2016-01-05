# Scripts for manipulating the release catalog

## Credentials

Some of the scripts require Github credentials. The file at 

    (build-path (find-system-path 'prefs-dir) "github-user-credentials.rktd")

should contain a Github "personal access token" (see Settings,
Personal access tokens) with the authority to manipulate the relevant
repositories. Specifically, `read`ing from the file should produce a
string containing the token.

Even scripts that don't write to repositories may need a token to
avoid hitting the rate limit for unauthenticated applications.

## Typical usage

The following outlines the usual use of the scripts.

At the beginning of a release cycle, create a release catalog, filter
it to the relevant pkgs, and make sure it's up to date:

- raco pkg catalog-copy https://pkg.racket-lang.org release-catalog
- racket scripts/filter.rkt -fi release-catalog main-distribution main-distribution-test distro-build
- racket scripts/checksum.rkt -fi release-catalog

During the release, periodically poll for new release branches and
updates to existing release branches:

- racket scripts/poll.rkt -fi release-catalog

For the final build, turn the release branches into version tags,
redirect the catalog sources, and delete the release branches:

- racket scripts/add-tags.rkt -i release-catalog --from-branch release --tag v$VERSION
- racket scripts/redirect.rkt -fi release-catalog --from-branch release --branch v$VERSION
- racket scripts/delete-branches.rkt -i release-catalog --from-branch v$VERSION --branch release
