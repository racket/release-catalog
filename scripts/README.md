# Scripts for manipulating the release catalog

## Credentials

Some of the scripts require Github credentials. The file at 

    (build-path (find-system-path 'pref-dir) "github-user-credentials.rktd")

should contain a Github "personal access token" (see Settings,
Personal access tokens) with the authority to manipulate the relevant
repositories. Specifically, `read`ing from the file should produce a
string containing the token.

Even scripts that don't write to repositories may need a token to
avoid hitting the rate limit for unauthenticated applications.

`scripts/poll.rkt` (and possibly others, too) instead use an application key
(to be used from scripts on the build server with the build account,
presumably --stamourv). It should live at

    (build-path (find-system-path 'pref-dir) "github-poll-client.rktd")

and contain a list of two strings, the client id and the client secret.
(I'm not entirely sure where those come from, possibly generated when
registering an application (here the release-catalog polling client)
with GitHub. (The write aspects of the build script that also does the
polling is handled differently, via a "deploy key" on the build server.)
If you need them, I (and probably ryanc) have them. --stamourv)

## Typical usage

(Full details are in the release checklist, master copy in the iplt repo.)

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
