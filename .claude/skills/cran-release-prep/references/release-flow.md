# Submission, branching and the GitHub release

Read this once the branch is verified clean. It covers what happens after the
package itself is ready: submitting, merging, tagging, and keeping the repo's
record of releases honest.

## Submit

```r
devtools::submit_cran()
```

This writes `CRAN-SUBMISSION`:

```
Version: 0.7.0
Date: 2026-09-14 18:56:48 UTC
SHA: 722736ed6598abff399ce6fab1b598cb7b23d51d
```

**Commit and push that file.** It is the only record of which tree went to CRAN,
and it is what lets anyone later prove what was submitted. In one repo the
commits submitted for two consecutive releases had never been pushed — those
trees existed on a single laptop, and reconstructing them meant downloading the
published tarballs and content-matching against rebased copies.

## Branch flow: merge the release branch to both main and develop

If the repo uses a `main` + `develop` split, merge the release branch into
**both**:

```
                 +--> main      (release merges only; tagged; == CRAN)
release branch --|
                 +--> develop   (back-merge, so develop keeps release fixes)
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
