#!/usr/bin/env bash
# Argument check for the just recipes that take arguments. Runs before anything is built, so a
# bad call fails in milliseconds with a hint. The programs still validate their own input; the flag
# names and the presets are read from the built CLI, so they cannot drift.
set -euo pipefail
recipe=$1
shift

settings="meridian"
# The preset names come from the CLI itself (`presets`) through the built program, so this list cannot
# drift from `Settings.Presets`. Before the first build there is no program; the CLI validates then.
cli() { node --input-type=module -e 'import("./output/Main/index.js").then((m) => m.main())' - "$@" 2>/dev/null | tr '\n' ' ' || true; }
presets=$(cli presets)
packages=$(just --evaluate packages)

case $recipe in
  sim)      usage="just sim <setting> $(cli usage run)"
            example="just sim meridian --preset Conservative --rounds 5"
            flags=$(cli flags run) positional=setting ;;
  session)  usage="just session <setting> $(cli usage session)"
            example="just session meridian --humans US --rounds 3"
            flags=$(cli flags session) positional=setting ;;
  serve)    usage="just serve $(cli usage serve)   (defaults: runs.db, 8787, apps/dashboard/dist)"
            example="just serve --port 9000"
            flags=$(cli flags serve) positional= ;;
  test-pkg) usage="just test-pkg <package>"
            example="just test-pkg core"
            flags= positional=package ;;
esac

fail() {
  echo "error: $1"
  echo
  echo "usage: $usage"
  case $positional in
    setting) echo "  settings: $settings" ;;
    package)  echo "  packages:  $packages" ;;
  esac
  [ "$recipe" = sim ] && echo "  presets:   $presets"
  echo
  echo "e.g. $example"
  exit 1
}

one_of() { [[ " $2 " == *" $1 "* ]]; }

if [ -n "$positional" ]; then
  [ $# -gt 0 ] && [[ "$1" != -* ]] || fail "missing $positional"
  case $positional in
    setting) one_of "$1" "$settings" || fail "unknown setting '$1'" ;;
    package)  one_of "$1" "$packages" || fail "unknown package '$1'"
              [ $# -eq 1 ] || fail "test-pkg takes one package" ;;
  esac
  shift
fi

seen=""
while [ $# -gt 0 ]; do
  flag=$1
  # before the first build the CLI cannot be asked; the program validates then
  [ -z "${flags// /}" ] || one_of "$flag" "$flags" || fail "unknown option '$flag'"
  one_of "$flag" "$seen" && fail "$flag given twice"
  seen="$seen $flag"
  [ $# -ge 2 ] || fail "$flag needs a value"
  value=$2
  case $flag in
    --rounds|--port) [[ "$value" =~ ^[1-9][0-9]*$ ]] || fail "$flag must be a whole number of at least 1, got '$value'" ;;
    --window)        [[ "$value" =~ ^[0-9]+$ ]] || fail "$flag must be a whole number, got '$value'" ;;
    --seed)          [[ "$value" =~ ^-?[0-9]+$ ]] || fail "$flag must be a whole number, got '$value'" ;;
    --preset)        [ -z "${presets// /}" ] || one_of "$value" "$presets" || fail "unknown preset '$value'" ;;
  esac
  shift 2
done
