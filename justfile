export PATH := justfile_directory() + "/node_modules/.bin:" + env_var('PATH')

packages := "core board policy scenarios effects handlers adapters harness cli server dashboard"

default:
    @just --list

# Compile every package
build:
    spago build

# Run every package's test suite
test: scenarios
    #!/usr/bin/env bash
    set -euo pipefail
    for p in {{packages}}; do spago test -p sp-$p; done

# Run one package's test suite, e.g. `just test-pkg core`
test-pkg name: scenarios
    spago test -p sp-{{name}}

# Format all PureScript sources
fmt:
    purs-tidy format-in-place "packages/*/src/**/*.purs" "packages/*/test/**/*.purs" "apps/*/src/**/*.purs" "apps/*/test/**/*.purs"

# Check formatting without writing
fmt-check:
    purs-tidy check "packages/*/src/**/*.purs" "packages/*/test/**/*.purs" "apps/*/src/**/*.purs" "apps/*/test/**/*.purs"

# Check package dependency direction (docs/07_architecture.md §3)
deps:
    node scripts/check-deps

# Compile the DSL scenarios (packages/scenarios/src/Scenarios) to their gitignored JSON
scenarios:
    spago run -p sp-scenarios --main Scenarios.Compile

# Format check, dependency check, build, and test
check: fmt-check deps build test

# All phase gates
all: check dashboard-test

# Run the CLI
run *args: scenarios
    spago run -p sp-cli -- {{args}}

# shadcn/ui components generated into apps/dashboard/ui (never committed; see ui.sha256)
ui_components := "card tabs select checkbox badge button input table chart sheet sidebar separator skeleton tooltip"

# Generate ui/ from the pinned shadcn CLI in a scratch directory, so the repo's package.json is untouched
_ui-generate:
    #!/usr/bin/env bash
    set -euo pipefail
    tmp=$(mktemp -d)
    trap 'rm -rf "$tmp"' EXIT
    cp apps/dashboard/components.json apps/dashboard/app.css apps/dashboard/tsconfig.json package.json "$tmp/"
    (cd "$tmp" && shadcn add -y -o {{ui_components}} > shadcn.log 2>&1) || { cat "$tmp/shadcn.log"; exit 1; }
    rm -rf apps/dashboard/ui
    cp -R "$tmp/ui" apps/dashboard/ui

# Ensure apps/dashboard/ui matches the committed checksums; generate it when absent or stale
ui:
    #!/usr/bin/env bash
    set -euo pipefail
    if [ -d apps/dashboard/ui ] && (cd apps/dashboard && shasum -a 256 -c ui.sha256 > /dev/null 2>&1); then exit 0; fi
    just _ui-generate
    (cd apps/dashboard && shasum -a 256 -c ui.sha256) || { echo "generated ui/ does not match apps/dashboard/ui.sha256: review the upstream change, then run 'just ui-accept'"; exit 1; }

# Regenerate ui/ from the pinned CLI and record its checksums as the new baseline
ui-accept:
    just _ui-generate
    cd apps/dashboard && find ui -type f | LC_ALL=C sort | xargs shasum -a 256 > ui.sha256

# Build the dashboard bundle (PureScript, then Vite)
dashboard: ui
    spago build -p sp-dashboard
    vite build --config apps/dashboard/vite.config.js

# Serve the read API and dashboard, e.g. `just serve --db runs.db`
serve *args: scenarios dashboard
    spago run -p sp-server -- {{args}}

# Playwright smoke test against a freshly seeded Run Store
dashboard-test: scenarios dashboard
    cd apps/dashboard && playwright test

clean:
    rm -rf output .spago dist
