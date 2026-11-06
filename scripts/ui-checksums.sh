#!/usr/bin/env bash
# Checksums for the generated shadcn output: apps/dashboard/ui/** and apps/dashboard/theme.css.
#
#   scripts/ui-checksums.sh record   write apps/dashboard/ui.sha256 from the files on disk
#   scripts/ui-checksums.sh check    fail unless the file list and every checksum match ui.sha256
#
# The file list is compared as well as the contents, so a stale or extra file fails the check.
# Portable: uses sha256sum where it exists (Linux, nix coreutils) and falls back to shasum -a 256
# (macOS, perl-based shasum elsewhere). Both print the same "<hash>  <path>" format.
set -euo pipefail

cd "$(dirname "$0")/../apps/dashboard"

if command -v sha256sum > /dev/null 2>&1; then
  hash_cmd=(sha256sum)
else
  hash_cmd=(shasum -a 256)
fi

# Every generated file, sorted in byte order so the list is the same on every machine.
listing() {
  { find ui -type f; echo theme.css; } | LC_ALL=C sort
}

manifest_listing() {
  awk '{ print $2 }' ui.sha256 | LC_ALL=C sort
}

case "${1:-}" in
  record)
    listing | xargs "${hash_cmd[@]}" > ui.sha256
    ;;
  check)
    if [ ! -f ui.sha256 ]; then
      echo "apps/dashboard/ui.sha256 is missing" >&2
      exit 1
    fi
    if ! diff <(listing) <(manifest_listing) > /dev/null; then
      echo "apps/dashboard: generated files differ from ui.sha256 (added, removed or renamed):" >&2
      diff <(listing) <(manifest_listing) >&2 || true
      exit 1
    fi
    "${hash_cmd[@]}" -c ui.sha256
    ;;
  *)
    echo "usage: $0 record|check" >&2
    exit 2
    ;;
esac
