# Releasing badp

How a version reaches CRAN, `main` and the
[releases page](https://github.com/badp-project/badp/releases).

## Why this document exists

Two things had gone wrong, and they share a cause.

The releases page stopped at 0.3.0 while CRAN had shipped 0.4.0, 0.4.0.1, 0.5.0,
0.6.0 and 0.6.1. `main` sat at 0.4.0.1 the whole time. Every release since then
was submitted to CRAN from a branch that only ever reached `develop`.

Routing a release to `main` through `develop` cannot keep `main` equal to CRAN.
Work that lands on `develop` after the submission rides along into `main` under
the released version's number. This is not hypothetical: `ee0fc20`, the whole
0.7.0 rewrite (109 files), landed on `develop` after 0.6.1 was submitted. A
`develop` -> `main` PR opened after that would have set `main` to 0.6.1's version
number while carrying 0.7.0's code.

## The flow

The release branch reaches **both** `main` and `develop`.

```
                 +--> release-<version>-to-main --> main  (tagged; == CRAN)
release branch --|
                 +--> develop                             (back-merge, keeps release fixes)
```

`main` only ever receives release content, so its tree is the tree that went to
CRAN. `develop` carries on as the integration branch.

`main` is reached through a short-lived delivery branch rather than by opening a
PR from the release branch straight into `main`. That detour is forced by the
branch rules, not a matter of taste — see
[Promoting a release branch to `main`](#promoting-a-release-branch-to-main).

The release -> `main` PR is small and reviewable — it is one release, not
months of accumulated integration work — which is the other reason not to route
releases through a large `develop` -> `main` PR.

## Checklist

**On the release branch**

- [ ] Cut `release-<version>` from `develop`, e.g. `release-0.8.0`. The delivery
      branch used later is then `release-<version>-to-main`.
- [ ] Confirm `Version:` in `DESCRIPTION`. The bump often already happened on
      `develop` - check before changing it rather than assuming.
- [ ] Add the `NEWS.md` section. The heading must be exactly `# badp <version>` —
      the workflow uses that section verbatim as the release notes and fails
      without it. (0.4.0.1 has no section, which is why its notes had to be
      written by hand.)
- [ ] Add a `cran-comments.md` section. State the real check results, and the R
      version and platform the check actually ran on.
- [ ] `Rscript -e 'roxygen2::roxygenise()'` — `man/` and `NAMESPACE` must come out with no diff.
- [ ] `Rscript -e 'testthat::test_local()'`
- [ ] `R CMD build` + `R CMD check --as-cran`.
- [ ] Re-knit the README if `README.Rmd` or any output it shows changed.
      `README.md` is generated, never hand-edited:
      `Rscript -e 'devtools::build_readme()'`
- [ ] `Rscript -e 'spelling::spell_check_package()'`
- [ ] `Rscript -e 'pkgdown::check_pkgdown()'`

**Submit**

- [ ] `Rscript -e 'devtools::submit_cran()'`
- [ ] Commit **and push** the `CRAN-SUBMISSION` file it writes. It records the
      exact SHA submitted, the release job checks `main` against it, and it is
      the only record of which tree went to CRAN. The trees submitted for 0.6.0
      and 0.6.1 were never pushed anywhere.

**Merge — both targets, once CRAN has accepted**

Wait for the acceptance e-mail. Merging to `main` cuts the GitHub release, so
promoting earlier publishes a release for a version CRAN may still reject.

- [ ] PR the release branch into `develop`. This is not optional: the
      `cran-comments.md` section, NEWS edits, regenerated docs and
      `CRAN-SUBMISSION` exist only on the release branch, so skipping it means
      the next release is cut from a `develop` missing a version's history.
- [ ] Promote the release branch to `main` through a delivery branch — see
      [Promoting a release branch to `main`](#promoting-a-release-branch-to-main).
      Merging that PR cuts the tag and the GitHub release automatically.
- [ ] Do **not** reach `main` via a `develop` -> `main` PR. That is what broke
      the correspondence between `main` and CRAN.

**After both merges**

- [ ] Confirm the release exists and `main` shows the published version.
- [ ] If CRAN asks for changes, fix them on the release branch, bump to the next
      patch version, and submit again. Do not amend a published release.

## Promoting a release branch to `main`

### Why the release branch cannot be PRed into `main` directly

`main` and `develop` are both covered by rulesets (`main-branch-rules`,
`develop-branch-rules`) that require a PR, require linear history, block force
pushes, and list **no bypass actors**. Required linear history means no merge
commit can ever land on either branch, so every release PR into `main` has been
squashed.

A squash produces a commit with no ancestry link to the branch it came from.
`main` has therefore never been an ancestor of `develop`, and the two have not
shared a merge base since `09b9a28` (0.3.0, October 2025). `main` carries one
squash commit; a branch cut from `develop` carries every commit since 0.3.0.
Git replays both sides against that ancient base, so the two sets of changes
collide: roughly 53 conflicting files, including the binary `data/*.rda` objects
that can never auto-merge.

Cutting the release branch from `develop` does not avoid this. The release
branch inherits the divergence.

There is no merge button that fixes it. "Create a merge commit" is disabled by
the linear-history rule, and "Rebase and merge" rewrites commit SHAs, so the
rebased copies are not the release branch's commits either.

What remains is a single rule:

> A PR into `main` is conflict-free if and only if its head branch is a
> descendant of `main`.

### The recipe

Build a branch off `main` and set its tree to the release branch's tree:

```sh
git fetch origin
git checkout -b release-<version>-to-main origin/main
git read-tree -u --reset origin/<release branch>
git commit -m "Version <version> release"
git push -u origin release-<version>-to-main
gh pr create --base main --head release-<version>-to-main \
  --title "Version <version> release"
```

The result is one single-parent commit, on top of `main`, whose tree is
byte-identical to the release branch. Linear history is satisfied, the PR shows
no conflicts, and the diff GitHub renders is the real release diff. The guard
passes by construction, because the tree being compared *is* the submitted tree.

### Things that will bite

- **Use `read-tree`, not `merge`.** `git merge` reports conflicts that are all
  spurious — the release tree is strictly newer than `main`'s — and a merge
  commit would be rejected by the linear-history rule anyway.
- **`read-tree -u --reset` handles deletions.** `git checkout <branch> -- .`
  copies files in but never removes files that the release dropped, which would
  leave `main` with stale leftovers and fail the guard.
- **Do not delete the release branch after merging.** The guard resolves the SHA
  recorded in `CRAN-SUBMISSION`; if that commit becomes unreachable, every later
  release job fails.
- **The release branch must carry `.github/workflows/`.** Two files matter:
  `R-CMD-check-main.yaml` provides the five status checks `main` requires, and
  `release.yaml` is what cuts the tag. A `push` workflow runs from the file on
  the branch that was pushed, so if `release.yaml` is not on `main`, no release
  is ever cut. Both arrive automatically in any branch cut from `develop` after
  September 2026; the 0.7.0 branch predated them and had to have them added by
  hand.
- **`git switch` does not exist before git 2.23.** Use `git checkout -b`.
- **Old git may fail the push with `RPC failed; HTTP 400`.** git 2.22 with a
  modern curl negotiates HTTP/2 and GitHub rejects the upload. Fix with
  `git config --global http.version HTTP/1.1`, or upgrade git.

### What this does not fix

`main` and `develop` still share no ancestry, and they never will while both
require linear history. That is fine: nothing depends on it once releases reach
`main` this way, and `git diff main origin/<release branch>` stays the honest
check of whether `main` is what shipped.

If the constraint is ever relaxed — dropping `required_linear_history` from
`main-branch-rules` alone — release branches could be merged into `main` as true
merge commits and the delivery branch would no longer be needed. That is a
repository-settings decision and needs an admin.

## Tooling

`.claude/skills/cran-release-prep/` holds this process in a form Claude Code
picks up automatically: open the repo, ask it to prepare the release, and it
works through the checklist above. It is checked in so everyone gets the same
process rather than each of us remembering a different half of it.

Two of its scripts are plain bash and useful on their own:

```sh
# README.md vs README.Rmd, and man/ + NAMESPACE vs the roxygen comments.
# Catches drift that R CMD check cannot see, because check never reads the README.
.claude/skills/cran-release-prep/scripts/check_generated_in_sync.sh

# Which commit's content matches a version published on CRAN.
# Use when anchoring a tag, or to prove what was actually submitted.
.claude/skills/cran-release-prep/scripts/match_cran_tarball.sh badp 0.6.0 origin/develop
```

`.claude/` is in `.Rbuildignore`, so none of it reaches the tarball.

## The guard

Before publishing, the release job compares `main` against the commit recorded in
`CRAN-SUBMISSION`, over the paths that end up in the tarball (`R/`, `man/`,
`data/`, `tests/`, `inst/`, `vignettes/`, `NAMESPACE`, `DESCRIPTION`,
`NEWS.md`). If they differ, it fails and prints the diff rather than publishing a
release that misrepresents what CRAN got.

The usual cause of that failure is a release reaching `main` through `develop`
and picking up unrelated work. `cran-comments.md`, `renv.lock` and `.github/` are
build-ignored and excluded from the comparison.

## When a release is missing

`release.yaml` runs on a `DESCRIPTION` change on `main`, and weekly as a drift
check. If a release did not appear:

- Run it by hand: Actions -> `release.yaml` -> **Run workflow**. It does nothing
  when the release already exists, so re-running is always safe.
- If it failed, the usual causes are a missing `# badp <version>` section in
  `NEWS.md`, an unpushed `CRAN-SUBMISSION`, or the guard above.

The weekly drift check compares the version CRAN publishes against the releases
page and opens a `release-drift` issue when the page is behind.

To backfill a version by hand, anchor it to the commit that was actually
submitted — the SHA is in that version's `CRAN-SUBMISSION`. Where that commit was
never pushed, find the commit whose content matches the published tarball rather
than guessing from commit messages:

```sh
curl -sO https://cran.r-project.org/src/contrib/Archive/badp/badp_<version>.tar.gz
# compare R/, NAMESPACE, NEWS.md (add src/, tests/ to separate adjacent patches)
gh release create <version> --title "Version <version>" --target <sha> --notes-file <notes>
```

## Conventions

| Thing | Form | Example |
| --- | --- | --- |
| Tag | bare version, no `v` | `0.7.0` |
| Release title | `Version <version>` | `Version 0.7.0` |
| `NEWS.md` heading | `# badp <version>` | `# badp 0.7.0` |
| Release body | that `NEWS.md` section | — |

`renv.lock` is in `.Rbuildignore` and cannot affect CRAN, but keep it honest:
re-snapshot when dependencies move between `Imports` and `Suggests`.
