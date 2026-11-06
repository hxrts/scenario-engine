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
test-pkg name: _scenarios
    spago test -p sp-{{name}}

# Format all PureScript sources
fmt:
    purs-tidy format-in-place "packages/*/src/**/*.purs" "packages/*/test/**/*.purs" "apps/*/src/**/*.purs" "apps/*/test/**/*.purs"

# Check formatting without writing
_fmt-check:
    purs-tidy check "packages/*/src/**/*.purs" "packages/*/test/**/*.purs" "apps/*/src/**/*.purs" "apps/*/test/**/*.purs"

# Check package dependency direction
_deps:
    node scripts/check-deps

# Compile the DSL _scenarios to JSON
_scenarios:
    spago run -p sp-scenarios --main Scenarios.Compile

# fmt-check, deps, build, test
check: _fmt-check _deps build test

# check + dashboard smoke test
all: check _dashboard-test

# Simulate a game
sim *args: _scenarios
    spago run -p sp-cli -- run {{args}}

# Live facilitator session
session *args: _scenarios
    spago run -p sp-cli -- session {{args}}

# shadcn/ui components (apps/dashboard/ui) and their theme (apps/dashboard/theme.css): generated, never committed; see ui.sha256
ui_components := "theme-neutral card tabs checkbox button input chart sheet sidebar separator skeleton tooltip"

# Generate ui/ from the pinned shadcn CLI in a scratch directory, so the repo's package.json is untouched
_ui-generate:
    #!/usr/bin/env bash
    set -euo pipefail
    tmp=$(mktemp -d)
    trap 'rm -rf "$tmp"' EXIT
    cp apps/dashboard/components.json apps/dashboard/tsconfig.json package.json "$tmp/"
    printf '@import "tailwindcss";\n' > "$tmp/theme.css"
    (cd "$tmp" && shadcn add -y -o {{ui_components}} > shadcn.log 2>&1) || { cat "$tmp/shadcn.log"; exit 1; }
    rm -rf apps/dashboard/ui
    cp -R "$tmp/ui" apps/dashboard/ui
    sed '/^@import "tailwindcss";$/d' "$tmp/theme.css" > apps/dashboard/theme.css

# Generate ui/ if missing or stale
_ui:
    #!/usr/bin/env bash
    set -euo pipefail
    if [ -d apps/dashboard/ui ] && (cd apps/dashboard && shasum -a 256 -c ui.sha256 > /dev/null 2>&1); then exit 0; fi
    just _ui-generate
    (cd apps/dashboard && shasum -a 256 -c ui.sha256) || { echo "generated ui/ does not match apps/dashboard/ui.sha256: review the upstream change, then run 'just ui-accept'"; exit 1; }

# Regenerate ui/ and accept checksums
ui-accept:
    just _ui-generate
    cd apps/dashboard && (find ui -type f; echo theme.css) | LC_ALL=C sort | xargs shasum -a 256 > ui.sha256

# Build the _dashboard bundle
_dashboard: _ui
    spago build -p sp-dashboard
    vite build --config apps/dashboard/vite.config.js

# Serve the API and dashboard
serve *args: _scenarios _dashboard
    spago run -p sp-server -- {{args}}

# Dashboard smoke test (Playwright)
_dashboard-test: _scenarios _dashboard
    cd apps/dashboard && playwright test

_clean:
    rm -rf output .spago dist apps/dashboard/dist apps/dashboard/ui apps/dashboard/theme.css
