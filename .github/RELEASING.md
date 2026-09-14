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

The release branch is merged into **both** `main` and `develop`.

```
                 +--> main      (release merge only; tagged; == CRAN)
release branch --|
                 +--> develop   (back-merge, so develop keeps release fixes)
```

`main` only ever receives release merges, so its tree is the tree that went to
CRAN. `develop` carries on as the integration branch.

The release -> `main` PR is small and reviewable — it is one release, not
months of accumulated integration work — which is the other reason not to route
releases through a large `develop` -> `main` PR.

## Checklist

**On the release branch**

- [ ] Cut the branch from `develop`.
- [ ] Bump `Version:` in `DESCRIPTION`.
- [ ] Add the `NEWS.md` section. The heading must be exactly `# badp <version>` —
      the workflow uses that section verbatim as the release notes and fails
      without it. (0.4.0.1 has no section, which is why its notes had to be
      written by hand.)
- [ ] Add a `cran-comments.md` section. State the real check results, and the R
      version and platform the check actually ran on.
- [ ] `just document` — `man/` and `NAMESPACE` must come out with no diff.
- [ ] `just test`
- [ ] `just check` — `R CMD build` + `R CMD check --as-cran`.
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

**Merge — both targets, from the release branch**

- [ ] PR the release branch into `main`. Merging it cuts the tag and the GitHub
      release automatically.
- [ ] PR the release branch into `develop`.
- [ ] Do **not** reach `main` via a `develop` -> `main` PR. That is what broke
      the correspondence between `main` and CRAN.

**After CRAN accepts**

- [ ] Confirm the release exists and `main` shows the published version.
- [ ] If CRAN asks for changes, fix them on the release branch, bump to the next
      patch version, and submit again. Do not amend a published release.

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
