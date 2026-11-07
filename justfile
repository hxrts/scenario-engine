export PATH := justfile_directory() + "/node_modules/.bin:" + env_var('PATH')

packages := "core board policy scenarios effects handlers adapters harness cli server dashboard"

_default:
    @just --list

# Compile every package
build:
    spago build

# Run every package's test suite
test: _scenarios
    #!/usr/bin/env bash
    set -euo pipefail
    for p in {{packages}}; do spago test -p sp-$p; done

# Test one package
[positional-arguments]
test-pkg *args:
    @scripts/validate test-pkg "$@"
    @just _scenarios
    spago test -p sp-"$1"

# Format all PureScript sources
fmt:
    purs-tidy format-in-place "packages/*/src/**/*.purs" "packages/*/test/**/*.purs" "apps/*/src/**/*.purs" "apps/*/test/**/*.purs"

# Check formatting without writing
_fmt-check:
    purs-tidy check "packages/*/src/**/*.purs" "packages/*/test/**/*.purs" "apps/*/src/**/*.purs" "apps/*/test/**/*.purs"

# Check package dependency direction
_deps:
    node scripts/check-deps

# Compile the DSL scenarios to JSON
_scenarios:
    spago run -p sp-cli --main Compile

# fmt-check, deps, build, test
check: _fmt-check _deps build test

# check + dashboard smoke test
all: check dashboard-test

# Simulate a game
[positional-arguments]
sim *args:
    @scripts/validate sim "$@"
    @just _scenarios
    spago run -p sp-cli -- run "$@"

# Live facilitator session
[positional-arguments]
session *args:
    @scripts/validate session "$@"
    @just _scenarios
    spago run -p sp-cli -- session "$@"


# shadcn/ui components (apps/dashboard/ui) and their theme (apps/dashboard/theme.css) are generated,
# never committed. Their checksums are the baseline in apps/dashboard/ui.sha256 (scripts/ui-checksums.sh).
#
# components.json: the `utils`, `lib` and `hooks` aliases name paths that nothing generates. The
# generated files import `cn` from the npm package `cn` (a dependency in package.json), not from
# @/lib/utils, and the only alias the output uses is `@/ui`. The aliases stay because the shadcn CLI
# expects them; they are not references to files in this repo.
ui_components := "theme-neutral card tabs checkbox button input chart sheet sidebar separator skeleton tooltip"

# Generate ui/ from the pinned shadcn CLI in a scratch directory, so the repo's package.json is untouched.
# The lock file is copied too, so the CLI resolves the same dependency versions every time.
_ui-generate:
    #!/usr/bin/env bash
    set -euo pipefail
    tmp=$(mktemp -d)
    trap 'rm -rf "$tmp"' EXIT
    cp apps/dashboard/components.json apps/dashboard/tsconfig.json package.json package-lock.json "$tmp/"
    printf '@import "tailwindcss";\n' > "$tmp/theme.css"
    (cd "$tmp" && shadcn add -y -o {{ui_components}} > shadcn.log 2>&1) || { cat "$tmp/shadcn.log"; exit 1; }
    rm -rf apps/dashboard/ui
    cp -R "$tmp/ui" apps/dashboard/ui
    sed '/^@import "tailwindcss";$/d' "$tmp/theme.css" > apps/dashboard/theme.css

# Generate ui/ if missing or stale
_ui:
    #!/usr/bin/env bash
    set -euo pipefail
    if [ ! -d apps/dashboard/ui ]; then just _ui-generate; fi
    if ! scripts/ui-checksums.sh check; then
      echo "apps/dashboard/ui does not match apps/dashboard/ui.sha256. If the upstream change is intended, run 'just ui-accept'. Otherwise run 'rm -rf apps/dashboard/ui' and 'just ui'."
      exit 1
    fi

# Regenerate ui/ and accept checksums
ui-accept:
    just _ui-generate
    scripts/ui-checksums.sh record

# Build the _dashboard bundle
_dashboard: _ui
    #!/usr/bin/env bash
    set -euo pipefail
    spago build -p sp-dashboard
    # the bundle is rebuilt only when something it is built from is newer than it
    if [ -f apps/dashboard/dist/index.html ] && [ -z "$(find apps/dashboard/src apps/dashboard/ui apps/dashboard/app.css apps/dashboard/theme.css apps/dashboard/index.html apps/dashboard/main.js apps/dashboard/vite.config.mjs apps/dashboard/ports.mjs output/Dashboard* -newer apps/dashboard/dist/index.html -print -quit)" ]; then
      echo "dashboard bundle is up to date"
      exit 0
    fi
    vite build --config apps/dashboard/vite.config.mjs

# Serve the API and dashboard
[positional-arguments]
serve *args:
    @scripts/validate serve "$@"
    @just _scenarios _dashboard
    @scripts/free-port "$@"
    spago run -p sp-server -- "$@"

# One-off, billed live check of the joint pass: one real model call (docs/06 §14). Not in any suite.
live-interpret:
    #!/usr/bin/env bash
    set -euo pipefail
    if [ "${ALLOW_BILLED_LIVE:-}" != "1" ]; then echo "refused: this makes a billed model call; set ALLOW_BILLED_LIVE=1"; exit 2; fi
    just _scenarios
    spago test -p sp-adapters -m Test.Adapters.LiveInterpret

# Dashboard smoke test (Playwright). Needs the browser binaries once: `cd apps/dashboard && npx playwright install`.
dashboard-test: _scenarios _dashboard
    cd apps/dashboard && playwright test

_clean:
    rm -rf output .spago dist apps/dashboard/dist apps/dashboard/ui apps/dashboard/theme.css
