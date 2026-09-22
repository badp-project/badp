# Submission, branching and the GitHub release

Read this once the branch is verified clean. It covers what happens after the
package itself is ready: submitting, merging, tagging, and keeping the repo's
record of releases honest.

## Submit — the maintainer's job, not yours

CRAN sends a confirmation e-mail to the maintainer address, so the submission
has to be run by the person who can answer it. Prepare the branch, push it, and
hand off with the command rather than running it:

```r
devtools::submit_cran()
```

Once they confirm it is submitted, commit and push the `CRAN-SUBMISSION` file it
writes:

```
Version: 0.7.0
Date: 2026-09-14 18:56:48 UTC
SHA: 722736ed6598abff399ce6fab1b598cb7b23d51d
```

Pushing it is the part people skip. It is the only record of which tree went to
CRAN, and what lets anyone later prove what was submitted. In one repo the
commits submitted for two consecutive releases had never been pushed — those
trees existed on a single laptop, and reconstructing them meant downloading the
published tarballs and content-matching against rebased copies.

## Branch flow: the release branch must reach both main and develop

If the repo uses a `main` + `develop` split, the release branch has to land on
**both**:

```
                 +--> release-<version>-to-main --> main  (tagged; == CRAN)
release branch --|
                 +--> develop                             (back-merge)
```

Routing the release to `main` *through* `develop` cannot keep `main` equal to
CRAN. Anything that lands on `develop` after the submission rides along into
`main` under the released version's number.

This is a real failure, not a theoretical one. In one repo a 109-file rewrite
landed on `develop` after the previous version was submitted; a `develop → main`
PR opened afterwards would have labelled `main` with the old version while
shipping the new code. Meanwhile `main` sat three releases behind CRAN for
months because the big integration PR was waiting on a reviewer.

The secondary benefit is reviewability: a release → `main` PR is one release,
not months of accumulated integration work, so it actually gets approved.

## Getting the release branch onto main when branch rules block a PR

Do not assume a PR from the release branch into `main` will just work. Check the
shape of the history first:

```sh
git merge-base --is-ancestor origin/main origin/<release branch> \
  && echo "descendant - a normal PR works" \
  || echo "diverged - a direct PR will conflict"
```

**Why it diverges.** Where `main` requires linear history, every PR into it gets
squashed, and a squash produces a commit with no ancestry link to the branch it
came from. So `main` drifts out of the ancestry of `develop` permanently, one
release at a time. In this repo the two have shared no merge base since 0.3.0:
`main` carries a single squash commit while a branch cut from `develop` carries
eighteen months of history, and Git replays both against that ancient base —
~53 conflicting files, including binary `data/*.rda` that can never auto-merge.

No merge button avoids it. Linear history disables the merge commit, and
rebase-and-merge rewrites the SHAs, so the rebased copies are not the release
branch's commits any more. What is left is one rule:

> A PR into `main` is conflict-free if and only if its head branch is a
> descendant of `main`.

**The recipe.** Build a delivery branch off `main` and set its tree to the
release branch's tree:

```sh
git fetch origin
git checkout -b release-<version>-to-main origin/main
git read-tree -u --reset origin/<release branch>
git commit -m "Version <version> release"
git push -u origin release-<version>-to-main
gh pr create --base main --head release-<version>-to-main \
  --title "Version <version> release"
```

The result is one single-parent commit on top of `main` whose tree is
byte-identical to the release branch. Linear history is satisfied, the PR shows
no conflicts, and the diff GitHub renders is the real release diff. Any
submitted-tree guard passes by construction, because the tree being compared
*is* the submitted tree.

**Things that bite:**

- **`read-tree`, not `merge`.** `git merge` reports conflicts that are all
  spurious — the release tree is strictly newer — and the merge commit would be
  rejected by the linear-history rule anyway.
- **`-u --reset` is what handles deletions.** `git checkout <branch> -- .` copies
  files in but never removes files the release dropped, leaving stale leftovers
  on `main` that fail the guard.
- **Do not delete the release branch after merging.** A guard that resolves the
  SHA in `CRAN-SUBMISSION` fails forever once that commit is unreachable.
- **The release branch must carry `.github/workflows/`.** A `push` workflow runs
  from the file on the branch that was pushed, so a release workflow that is not
  on `main` never cuts anything. The same applies to whatever provides `main`'s
  required status checks. A release branch cut before those workflows existed
  needs them added by hand.
- **Old git.** `git switch` needs 2.23+; use `git checkout -b`. git 2.22 with a
  modern curl can fail the push with `RPC failed; HTTP 400` — fix with
  `git config --global http.version HTTP/1.1`, or upgrade.

**The real fix is a settings change.** Dropping `required_linear_history` from
the `main` ruleset alone would let release branches merge as true merge commits
and retire the delivery branch entirely. That needs a repo admin, so treat the
recipe above as the workaround it is, and say so rather than presenting it as
the natural way to ship.

## Tag and release conventions

Check what the repo already does before inventing anything (`git tag -l`,
`gh release list`). A common set:

| Thing | Form |
| --- | --- |
| Tag | bare version, no `v` prefix — `0.7.0` |
| Release title | `Version 0.7.0` |
| Release body | that version's `NEWS.md` section, verbatim |

Anchor the tag to the commit that was actually submitted, not to whatever is
current. Publish the release only after CRAN accepts — a version CRAN rejects
should not have a release implying it shipped.

## Automating it

A workflow that creates the release when a version bump lands on `main` keeps
the releases page in step without anyone remembering to do it. Two details make
the difference between automation that helps and automation that lies:

**Verify before publishing.** Compare the tree being released against the commit
recorded in `CRAN-SUBMISSION`, over the paths that end up in the tarball
(`R/`, `man/`, `data/`, `tests/`, `inst/`, `vignettes/`, `NAMESPACE`,
`DESCRIPTION`, `NEWS.md`). Exclude build-ignored paths — `cran-comments.md`,
`renv.lock`, `.github/` — which legitimately differ. If they diverge, fail and
print the diff rather than publishing a release that misrepresents what CRAN
received. This is what catches a release that reached `main` the wrong way.

**Fail loudly on a missing NEWS section.** If release notes come from
`# <pkg> <version>` in `NEWS.md` and that section is absent, stopping with a red
build is better than publishing an empty release. A red build gets noticed; a
silently skipped release does not.

Add a scheduled drift check as the safety net: compare the version CRAN
publishes against the releases page, and open an issue when the page is behind.

```sh
curl -fsSL https://cran.r-project.org/web/packages/<pkg>/DESCRIPTION \
  | sed -n 's/^Version:[[:space:]]*//p' | head -n1
```

## Backfilling missed releases

Use `scripts/match_cran_tarball.sh` to find the commit whose content matches
each published version, then:

```sh
gh release create <version> --title "Version <version>" \
  --target <sha> --notes-file <notes> --latest=false
```

Create them oldest first and mark only the current CRAN version as latest.

Two traps worth knowing:

- **Adjacent patch releases can be identical** across `R/`, `NAMESPACE` and
  `NEWS.md` — one pair differed only in `src/` and a test tolerance. Widen with
  `EXTRA_PATHS="src tests"` to separate them.
- **A same-content commit made after the publication date is the wrong anchor.**
  Prefer the matching commit dated on or before the CRAN publication date, which
  the archive listing gives you:

```sh
curl -s https://cran.r-project.org/src/contrib/Archive/<pkg>/
```

Where a version has no `NEWS.md` section, write its notes from that version's
`cran-comments.md` entry instead.

## If CRAN rejects

Fix on a branch, bump to the next patch version, and submit again. Do not amend
a published release or reuse a version number — CRAN treats each version as
immutable, and so should the repo.
