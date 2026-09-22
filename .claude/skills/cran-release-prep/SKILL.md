---
name: cran-release-prep
description: Prepare and verify an R package branch for a CRAN submission — bump the version, regenerate documentation, re-knit the README, run R CMD check --as-cran, and write cran-comments.md from the real check results. Use this whenever the user is getting an R package ready for CRAN: "prepare the CRAN release", "get 0.7.0 ready for CRAN", "cut a release", "submit to CRAN", "is this ready to submit", "run the release checks", or asks to update NEWS.md / cran-comments.md / DESCRIPTION for a new version. Also use when a CRAN submission was rejected and needs fixing, when checking whether a release branch is actually clean, or when the user asks what version is on CRAN versus in the repo. Applies to any R package with a DESCRIPTION file, not one specific repo.
---

# Preparing a CRAN release-ready branch

A CRAN submission is close to one-shot: a human reviews it, and a rejection costs days. Worse, whatever ships stays on CRAN. A broken example in a README is copied by every user who lands on the package page.

So the job is not "run the checks and see green". It is to establish that every derived file in the tree was actually **regenerated from its source**, and that every claim in `cran-comments.md` is **true of the run that just happened**.

## The failure shape to hunt for

Nearly every real defect found in this kind of work has the same shape:

> A file that is supposed to be generated from another file has drifted from it, and nobody noticed because the stale version still looks plausible.

`R CMD check` does not catch this. It checks the package, not whether `README.md` agrees with `README.Rmd`, or whether `cran-comments.md` describes the check that actually ran. Those need deliberate verification, and they are where the expensive mistakes live.

A worked example: in one release, `README.md` documented `best_models(bma_list = x, ...)` — an argument that release had *renamed*. The `.Rmd` was correct; the `.md` had been hand-edited and never re-knitted. It passed `R CMD check` cleanly, because check never runs the README. It would have shipped a copy-pasteable example that errors.

## The lifecycle, and the two places you stop

A release is not one task. It is three stretches of work separated by two waits
on the maintainer, and both waits are on things no agent can do: CRAN requires a
confirmation e-mail sent to the maintainer's address, and an acceptance decision
arrives hours or days later. Running past either one is the way this goes wrong.

```
  YOU                          MAINTAINER
  1 confirm version
  2 cut release-<version>
  3 prep + verify  ............. (SKILL.md sequence below)
  4 commit + push
  5 hand off  --------------->   submits to CRAN (e-mail confirmation)
  6 commit + push CRAN-SUBMISSION
     ..........................  waits for CRAN's decision
  7 back-merge to develop  <---  says CRAN accepted
  8 promote to main
                                 automation cuts the tag + GitHub release
```

**1. Confirm the version before creating anything.** Read what `develop`
carries — the bump often already happened there — and say which version you are
about to release. Do not infer it; a wrong guess names the branch, the NEWS
heading and the tag.

```sh
git show origin/develop:DESCRIPTION | sed -n 's/^Version:[[:space:]]*//p'
```

**2. Cut `release-<version>` from `develop`**, e.g. `release-0.8.0`. The delivery
branch used later is then `release-<version>-to-main`, which reads as an obvious
pair.

**3–4. Prep, verify, then commit and push.** Work the sequence below, commit the
result, and push the branch. Pushing matters beyond backup: the GitHub Actions
check matrix is the cross-platform evidence `cran-comments.md` claims, so it has
to have run on this commit before anyone submits.

**5. Stop and hand off.** Do not run `devtools::submit_cran()`. Submission
triggers a confirmation e-mail to the maintainer, so it is theirs to run. Report
that the branch is ready, what the check produced, and the command to run.

**6. When they say it is submitted**, commit and push `CRAN-SUBMISSION`. It
records the exact submitted SHA and is the only durable record of which tree
went to CRAN. Then stop again — CRAN has not decided yet.

**7–8. Only once CRAN accepts**, back-merge to `develop` and promote to `main`.

Two reasons the order matters. Merging to `main` fires the release automation,
so promoting before acceptance publishes a GitHub release for a version CRAN may
still reject. And the back-merge is not optional: the `cran-comments.md` section,
NEWS edits, regenerated docs and `CRAN-SUBMISSION` exist only on the release
branch, so skipping it means the next release is cut from a `develop` that has
silently lost a version's history.

Promotion to `main` has its own failure mode under branch rules — read
`references/release-flow.md` before attempting it.

## Sequence

This is step 3 above. Work in order; later steps assume earlier ones are clean.

### 0. Orient before changing anything

Read, don't assume:

```sh
cat DESCRIPTION | head -5          # current version
grep -n '^# ' NEWS.md | head       # which versions have entries
cat CRAN-SUBMISSION 2>/dev/null    # last submitted version + SHA
git log --oneline -10
git status --short
```

Then find out what CRAN actually has — the repo is not the authority:

```sh
curl -s https://cran.r-project.org/web/packages/<pkg>/DESCRIPTION | grep -E '^(Version|Date/Publication)'
curl -s https://cran.r-project.org/src/contrib/Archive/<pkg>/ | grep -o '<pkg>_[0-9.]*tar.gz'
```

This matters more than it sounds. A `cran-comments.md` in one repo claimed a version was "developed but never submitted"; the CRAN archive showed it published months earlier. Release notes had been written against that false premise.

Often the version bump and NEWS entry are already committed by a colleague. Check before you write — your job may be verification rather than authorship.

### 1. Make the roxygen version match

If `DESCRIPTION` has `Config/roxygen2/version: X`, install exactly that version before regenerating. A mismatch rewrites `NAMESPACE` formatting wholesale (roxygen 8.1 writes multi-line `importFrom`, 8.0 writes one per line) and buries any real change in noise.

```sh
grep 'Config/roxygen2/version' DESCRIPTION
Rscript -e 'packageVersion("roxygen2")'
```

If they differ, install the declared version rather than regenerating with what you have.

### 2. Regenerate documentation — expect a zero diff

```sh
Rscript -e 'roxygen2::roxygenise()'
git status --short
```

**No diff is the pass condition.** A diff means `man/` or `NAMESPACE` was out of sync with the roxygen comments — someone edited one without the other. Inspect what changed before accepting it.

### 3. Verify generated files against their generators

This is the step that catches what check cannot. Run:

```sh
scripts/check_generated_in_sync.sh
```

It compares the R code in `README.md` against the chunks in `README.Rmd`. They must be identical — `README.md` is knitted output, so if its code differs from its source, the file was hand-edited and **all of its output is stale**, including numbers.

That is a proof, not a heuristic: knitr copies chunk source verbatim into the output. Divergence is only possible if a human typed into the generated file.

When it reports drift, re-knit rather than hand-patch:

```sh
Rscript -e 'devtools::build_readme()'
```

**Fix environment artifacts at the source, not in the output.** If re-knitting injects noise like `Warning: package 'x' was built under R version 4.5.2`, that is your machine's binary, not the package. Reinstall that dependency from source so the warning stops being emitted. Editing it out of the generated file is a lie that returns on the next knit.

Expect some numeric churn in re-knitted output. Optimizer results can vary in the last digit across BLAS implementations. Say so in the summary rather than suppressing it; if the user wants it gone, the fix is a fixed seed or fixed starting values in the `.Rmd`, which is a change to make deliberately and not during a release.

### 4. Tests

```sh
Rscript -e 'devtools::test()'
```

### 5. Spelling

```sh
Rscript -e 'spelling::spell_check_package()'
```

Add genuinely new terms to `inst/WORDLIST` by hand. **Do not run `spelling::update_wordlist()`** during a release: it re-sorts the entire file (case-insensitive to ASCII order), producing dozens of lines of churn around a couple of real changes. Stale entries in a wordlist are harmless — they never cause a failure.

Often the better fix is in the prose. A package name flagged as a typo usually wants backticks anyway, and a code span is skipped by the checker, so no wordlist entry is needed.

### 6. pkgdown index

```sh
Rscript -e 'pkgdown::check_pkgdown()'
```

Catches exported functions missing from `_pkgdown.yml`, which breaks the site build after release.

### 7. Sweep for stale API references

If the release renames or removes anything, grep the whole tree — vignettes, README, `_pkgdown.yml`, tests:

```sh
grep -rn 'old_arg_name\|removed_component' --include=*.R --include=*.Rmd --include=*.Rd --include=*.md . | grep -v '^./renv/'
```

Expect hits in `NEWS.md` (historical entries, correct to keep) and in tests that assert the removal. Anything else is a live stale reference.

### 8. Audit declared Imports

Every package in `Imports:` should still be used, especially after a refactor that removed code:

```sh
for p in $(sed -n '/^Imports:/,/^[A-Z]/p' DESCRIPTION | grep -oE '^\s+[a-zA-Z.][a-zA-Z0-9.]*'); do
  echo "$p: $(grep -rc "${p}::" R/ | paste -sd+ - | bc) namespaced calls"
done
```

A package used only via `importFrom` in `NAMESPACE` shows zero here and is still fine — check both before concluding anything.

### 9. R CMD check --as-cran

```sh
R CMD build .
_R_CHECK_CRAN_INCOMING_=true _R_CHECK_CRAN_INCOMING_REMOTE_=true \
  R CMD check --as-cran <pkg>_<version>.tar.gz
```

This takes minutes — run it in the background and do other verification meanwhile.

Read the NOTEs individually. Environmental NOTEs (outdated HTML Tidy, unreachable time server, missing V8) are about the check machine, not the package, and belong in `cran-comments.md` with that explanation. Any other NOTE needs fixing or a real justification.

### 10. Write cran-comments.md from the run that just happened

A draft `cran-comments.md` written before the checks is a hypothesis. Verify every claim against the actual output:

- the NOTE text must be the NOTE that was produced, not one inherited from the previous release
- the R version and platform must be the ones checked on
- "0 errors | 0 warnings | N notes" must match the `Status:` line

Structure that works:

```markdown
# <version>

## Update
<what changed, breaking changes called out explicitly>

## R CMD check results
0 errors | 0 warnings | 1 note

* checking <exact check name> ... NOTE
  <exact text>

  <why this is environmental and unrelated to the package>

Checked locally with `R CMD check --as-cran` against R <version> on
<platform>, and via the GitHub Actions R-CMD-check workflow on Linux,
macOS and Windows across release, oldrel and devel.

## Downstream dependencies
<none, or the revdep check results>
```

Only claim the CI matrix if it has actually run on this commit. If it hasn't, push first or drop the sentence.

### 11. Verify the tarball matches the tree

Content edits made after `R CMD build` are not in the tarball. Extract and compare:

```sh
tar xzf <pkg>_<version>.tar.gz -C /tmp/verify
diff -q /tmp/verify/<pkg>/NEWS.md NEWS.md
```

`DESCRIPTION` differing is expected and fine — `R CMD build` reflows it and appends `Packaged:`/`Author:`.

## Judgment: necessary change versus churn

Release branches attract incidental edits. Before including a change, ask what breaks if it is left out. If the answer is "nothing", leave it out — a reviewer who finds churn in a release diff stops trusting the rest of it.

| Change | Include? |
| --- | --- |
| Re-knitted README after a real source change | Yes — the file was wrong |
| Wordlist re-sorted by a tool | No — revert; nothing required it |
| Rd markup (`\pkg{}`) in `NEWS.md` | Yes — NEWS renders as Markdown, so it displays literally on CRAN |
| Regenerated `man/` with no roxygen change | No — investigate the version mismatch instead |

When the user pushes back on a change, test the claim before defending it. Determine empirically whether a re-knit's numeric differences come from the environment (vary core count, rerun) or from stale content (compare the generated file's code against its source). The evidence settles it either way, and being wrong cheaply is better than arguing.

## After the branch is clean

Submission, branching, tagging and the GitHub release are a separate concern. Read `references/release-flow.md` before doing any of them, and especially before promoting a release to `main` or backfilling a tag — it carries recipes that are not guessable.

Two things from it worth knowing up front, because they are easy to get wrong irreversibly:

- `devtools::submit_cran()` writes `CRAN-SUBMISSION` recording the exact submitted SHA. Commit **and push** it; it is the only record of which tree went to CRAN, and release automation may resolve that SHA later. For the same reason, never delete a release branch after merging.
- Where `main` requires linear history, a PR from the release branch into `main` can fail with dozens of spurious conflicts, because squash merges leave `main` outside the ancestry of `develop`. Check with `git merge-base --is-ancestor origin/main <release branch>` before promising a clean merge; the reference has the delivery-branch recipe for when it is not.

## Scripts

- `scripts/check_generated_in_sync.sh` — compares README.md code against README.Rmd chunks and checks roxygen version alignment. Run early; it is fast and catches the expensive class of bug.
- `scripts/match_cran_tarball.sh` — given a version, downloads the published CRAN tarball and finds which commit's content matches it. Use when anchoring a git tag or GitHub release to a past release, or to prove what was actually submitted.
