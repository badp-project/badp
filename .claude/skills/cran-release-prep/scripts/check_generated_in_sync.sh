#!/usr/bin/env bash
# Check that generated files in an R package still agree with their generators.
#
# R CMD check never looks at these, so drift here survives a clean check and
# ships to CRAN. The README check in particular is a proof rather than a
# heuristic: knitr copies chunk source verbatim into the output, so if the code
# in README.md differs from the chunks in README.Rmd, a human edited the
# generated file and every number in it is stale.
#
# Usage: check_generated_in_sync.sh [package-root]
# Exit:  0 all checks passed, 1 drift found, 2 could not run

set -uo pipefail

ROOT="${1:-.}"
cd "$ROOT" || { echo "Cannot enter $ROOT"; exit 2; }

if [ ! -f DESCRIPTION ]; then
  echo "No DESCRIPTION in $(pwd) - not an R package root."
  exit 2
fi

FAIL=0
note()  { printf '  %s\n' "$1"; }
pass()  { printf 'PASS  %s\n' "$1"; }
fail()  { printf 'FAIL  %s\n' "$1"; FAIL=1; }
skip()  { printf 'SKIP  %s\n' "$1"; }

echo "Checking generated files in $(pwd)"
echo

# ---------------------------------------------------------------- roxygen ---
# A roxygen version mismatch rewrites NAMESPACE formatting wholesale and buries
# real changes in noise, so it is worth knowing before regenerating anything.
declared="$(sed -n 's/^Config\/roxygen2\/version:[[:space:]]*//p' DESCRIPTION | head -n1)"
if [ -z "$declared" ]; then
  skip "roxygen2 version not declared in DESCRIPTION"
else
  # tail -n1: startup banners (renv "project is out-of-sync", etc.) print to
  # stdout ahead of the value, and cat() leaves it on the final line.
  installed="$(Rscript -e 'cat("\n", as.character(tryCatch(packageVersion("roxygen2"), error = function(e) "")), sep = "")' 2>/dev/null | tail -n1 | tr -d '[:space:]')"
  if [ -z "$installed" ]; then
    skip "roxygen2 declared as $declared but not installed"
  elif [ "$declared" = "$installed" ]; then
    pass "roxygen2 version matches DESCRIPTION ($declared)"
  else
    fail "roxygen2 mismatch: DESCRIPTION says $declared, installed is $installed"
    note "Regenerating now would reformat NAMESPACE spuriously."
    note "Install $declared first: Rscript -e 'install.packages(\"roxygen2\")'"
  fi
fi

# ----------------------------------------------------------------- README ---
if [ ! -f README.Rmd ]; then
  skip "no README.Rmd (nothing to generate README.md from)"
elif [ ! -f README.md ]; then
  fail "README.Rmd exists but README.md does not - run devtools::build_readme()"
else
  tmp="$(mktemp -d)"
  trap 'rm -rf "$tmp"' EXIT

  # Compare in one direction only: every line of R code in README.md must also
  # appear somewhere in README.Rmd. Going the other way produces false alarms,
  # because plenty of source legitimately never reaches the output - chunks
  # marked include=FALSE or echo=FALSE, and the setup chunk. Nothing travels the
  # other way, though: knitr invents no code. So a code line present in the
  # output but absent from the source can only have been typed in by hand.
  comment="$(sed -n 's/.*comment[[:space:]]*=[[:space:]]*"\([^"]*\)".*/\1/p' README.Rmd | head -n1)"
  [ -z "$comment" ] && comment="#>"

  sed 's/[[:space:]]*$//' README.Rmd > "$tmp/rmd.txt"

  # Code inside ```r / ```{r} fences of the output, minus the comment-prefixed
  # result lines knitr adds. Bare punctuation carries no signal, so skip it.
  # Matches "``` r" (github_document), "```r" and "```{r}". Avoids \b, which
  # BSD/macOS awk does not support.
  awk '/^```+[[:space:]]*\{?[rR]([[:space:]}]|$)/ {inside=1; next} /^```+[[:space:]]*$/ {inside=0} inside' README.md \
    | grep -vF "$comment" \
    | sed 's/[[:space:]]*$//' \
    | grep -v '^[[:space:]]*$' \
    | grep -vE '^[[:space:]]*[])},;[({]*[[:space:]]*$' > "$tmp/md.txt"

  if [ ! -s "$tmp/md.txt" ]; then
    skip "no R code blocks found in README.md"
  else
    : > "$tmp/orphans.txt"
    while IFS= read -r line; do
      grep -qxF "$line" "$tmp/rmd.txt" || printf '%s\n' "$line" >> "$tmp/orphans.txt"
    done < "$tmp/md.txt"

    if [ ! -s "$tmp/orphans.txt" ]; then
      pass "every line of R code in README.md is present in README.Rmd"
    else
      fail "README.md contains R code that is not in README.Rmd"
      note "README.md is generated output and knitr invents no code, so these"
      note "lines were hand-edited in. Its printed results are stale too."
      note "Re-knit rather than hand-patching:"
      note "  Rscript -e 'devtools::build_readme()'"
      echo
      echo "  --- in README.md but not README.Rmd ---"
      sed 's/^/  /' "$tmp/orphans.txt" | head -25
    fi
  fi
fi

# ------------------------------------------------------- man/ + NAMESPACE ---
# Only meaningful inside a clean git tree: regenerate and see if anything moves.
if ! git rev-parse --git-dir >/dev/null 2>&1; then
  skip "not a git repository - cannot diff regenerated docs"
elif [ -n "$(git status --porcelain -- man NAMESPACE 2>/dev/null)" ]; then
  skip "man/ or NAMESPACE already modified - commit or stash, then re-run"
elif ! Rscript -e 'quit(status = !requireNamespace("roxygen2", quietly = TRUE))' 2>/dev/null; then
  skip "roxygen2 not installed - cannot verify man/ and NAMESPACE"
else
  Rscript -e 'suppressMessages(roxygen2::roxygenise())' >/dev/null 2>&1
  drift="$(git status --porcelain -- man NAMESPACE)"
  if [ -z "$drift" ]; then
    pass "man/ and NAMESPACE are in sync with the roxygen comments"
  else
    fail "man/ or NAMESPACE was out of sync - roxygenise() changed files"
    echo "$drift" | sed 's/^/    /'
    note "Inspect the diff, then commit it. If it is only NAMESPACE"
    note "reformatting, fix the roxygen2 version mismatch above instead."
  fi
fi

echo
if [ "$FAIL" -eq 0 ]; then
  echo "All generated files are in sync."
else
  echo "Drift found. Fix the FAIL items above before submitting."
fi
exit "$FAIL"
