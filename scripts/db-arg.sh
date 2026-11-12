#!/usr/bin/env bash
# Prints the store a `just serve` uses: the value of `--db` in the arguments, else the server's default, runs.db.
set -euo pipefail
db="runs.db"
prev=""
for arg in "$@"; do
  if [ "$prev" = "--db" ]; then db="$arg"; fi
  prev="$arg"
done
echo "$db"
