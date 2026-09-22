#!/usr/bin/env bash
# Find which commit's content matches a version published on CRAN.
#
# Useful when you need to know what was actually submitted: anchoring a git tag
# or GitHub release to a past version, or confirming that the commit recorded in
# CRAN-SUBMISSION really is the tree CRAN received. Commit messages lie about
# this; released tarballs do not.
#
# Matching is by content fingerprint over the paths R CMD build copies verbatim.
# DESCRIPTION is excluded because build reflows it and appends Packaged:/Author:.
#
# Usage: match_cran_tarball.sh <package> <version> [git-ref-to-search]
#   e.g. match_cran_tarball.sh badp 0.6.0 origin/develop
#        match_cran_tarball.sh badp 0.4.0 --all
#
# Default paths are R/ NAMESPACE NEWS.md. Adjacent patch releases often differ
# only in src/ or tests/ (0.4.0 and 0.4.0.1 were identical across the defaults),
# so set EXTRA_PATHS to separate them:
#   EXTRA_PATHS="src tests" match_cran_tarball.sh badp 0.4.0.1

set -uo pipefail

PKG="${1:?usage: match_cran_tarball.sh <package> <version> [git-ref|--all]}"
VERSION="${2:?usage: match_cran_tarball.sh <package> <version> [git-ref|--all]}"
REF="${3:---all}"
PATHS="R NAMESPACE NEWS.md ${EXTRA_PATHS:-}"

git rev-parse --git-dir >/dev/null 2>&1 || { echo "Not a git repository."; exit 2; }

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

TARBALL="${PKG}_${VERSION}.tar.gz"
echo "Locating $TARBALL on CRAN..."
for url in \
  "https://cran.r-project.org/src/contrib/$TARBALL" \
  "https://cran.r-project.org/src/contrib/Archive/$PKG/$TARBALL"
do
  if curl -fsS -o "$WORK/$TARBALL" "$url" 2>/dev/null; then
    echo "  downloaded from $url"
    break
  fi
done
[ -f "$WORK/$TARBALL" ] || { echo "Could not download $TARBALL from CRAN."; exit 2; }

tar xzf "$WORK/$TARBALL" -C "$WORK"

# Fingerprint = hash of (per-file hash + path), over PATHS, in sorted order.
fingerprint_dir() {
  ( cd "$1" && find $PATHS -type f 2>/dev/null | sort | xargs shasum 2>/dev/null | shasum | cut -d' ' -f1 )
}
fingerprint_commit() {
  git ls-tree -r "$1" --name-only -- $PATHS 2>/dev/null | sort | while read -r f; do
    printf '%s  %s\n' "$(git show "$1:$f" 2>/dev/null | shasum | cut -d' ' -f1)" "$f"
  done | shasum | cut -d' ' -f1
}

TARGET="$(fingerprint_dir "$WORK/$PKG")"
echo "Target fingerprint: $TARGET"
echo "Comparing paths:    $PATHS"
if [ "$REF" = "--all" ]; then
  echo "Searching:          all refs"
  REVS="$(git rev-list --all)"
else
  echo "Searching:          $REF"
  REVS="$(git rev-list "$REF")"
fi
echo

FOUND=0
while read -r sha; do
  [ -z "$sha" ] && continue
  if [ "$(fingerprint_commit "$sha")" = "$TARGET" ]; then
    ver="$(git show "$sha:DESCRIPTION" 2>/dev/null | sed -n 's/^Version:[[:space:]]*//p' | head -n1)"
    printf 'MATCH  %s  DESCRIPTION=%-10s %s\n' \
      "$(echo "$sha" | cut -c1-9)" "${ver:-?}" "$(git log -1 --format=%s "$sha")"
    printf '       %s\n' "$(git log -1 --format='%ai' "$sha")"
    FOUND=$((FOUND + 1))
  fi
done <<< "$REVS"

echo
if [ "$FOUND" -eq 0 ]; then
  echo "No commit matched. Things to try:"
  echo "  - widen the search: pass --all instead of a single ref"
  echo "  - the submitted commit may never have been pushed (check CRAN-SUBMISSION)"
  echo "  - a rebase may have rewritten it; content-identical copies can still differ"
  exit 1
fi

echo "$FOUND commit(s) matched."
if [ "$FOUND" -gt 1 ]; then
  echo
  echo "Several commits share this content - the release point is usually the"
  echo "newest, or whichever one updated CRAN-SUBMISSION. Narrow it with:"
  echo "  EXTRA_PATHS=\"src tests man data\" $0 $PKG $VERSION $REF"
  echo
  echo "Pick the commit dated on or before the CRAN publication date; a"
  echo "same-content commit made after publication is the wrong anchor."
fi
