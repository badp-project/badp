# Releasing badp

How a version reaches CRAN and the
[releases page](https://github.com/badp-project/badp/releases).

## Why this document exists

The releases page stopped at 0.3.0 while CRAN had shipped 0.4.0, 0.4.0.1, 0.5.0,
0.6.0 and 0.6.1. `main` sat at 0.4.0.1 for the same reason: the release lands on
`develop` quickly, but the `develop` -> `main` PR waits on a reviewer, and
everything downstream of that merge waited with it.

So the release automation triggers on **`develop`**, not `main`. A version gets
its GitHub release when it is ready, not when a reviewer gets to the PR.

`main` should still match the current CRAN version — that is the point of the
`develop` -> `main` PR, and the drift check below is there to catch it when it
slips.

## The flow

```
release branch  ->  develop  ->  main
                      |
                      +-- release.yaml cuts the GitHub release here
```

## Checklist

**On the release branch**

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

**Merge to develop**

- [ ] Merge the release branch into `develop`.
- [ ] Confirm `release.yaml` created the release. If not, see below.

**Submit**

- [ ] `Rscript -e 'devtools::submit_cran()'`
- [ ] Commit **and push** the `CRAN-SUBMISSION` file it writes. That file records
      the exact SHA submitted and is the only record of which tree went to CRAN.
      The trees submitted for 0.6.0 and 0.6.1 were never pushed anywhere.

**After CRAN accepts**

- [ ] Open the `develop` -> `main` PR so `main` matches the CRAN version.
- [ ] If CRAN asks for changes, fix them on a branch and bump to the next patch
      version. Do not amend a published release.

## When a release is missing

`release.yaml` runs on a `DESCRIPTION` change on `develop`, and weekly as a
drift check. If a release did not appear:

- Run it by hand: Actions -> `release.yaml` -> **Run workflow**. It reads the
  version from `DESCRIPTION` at the chosen ref and does nothing when the release
  already exists, so re-running is always safe.
- If it failed, the usual cause is a missing `# badp <version>` section in
  `NEWS.md`.

The weekly drift check compares the version CRAN publishes against the releases
page and opens a `release-drift` issue when the page is behind.

To backfill a version by hand, anchor it to the commit that was actually
submitted — the SHA is in that version's `CRAN-SUBMISSION`. Where that commit was
never pushed, find the commit whose content matches the CRAN tarball rather than
guessing from commit messages:

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
