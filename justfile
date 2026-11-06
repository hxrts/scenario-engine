export PATH := justfile_directory() + "/node_modules/.bin:" + env_var('PATH')

packages := "core board policy scenarios effects handlers adapters harness cli server dashboard"

default:
    @just --list

# Compile every package
build:
    spago build

# Run every package's test suite
test:
    #!/usr/bin/env bash
    set -euo pipefail
    for p in {{packages}}; do spago test -p sp-$p; done

# Run one package's test suite, e.g. `just test-pkg core`
test-pkg name:
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

# Format check, dependency check, build, and test
check: fmt-check deps build test

# All phase gates
all: check dashboard-test

# Run the CLI
run *args:
    spago run -p sp-cli -- {{args}}

# Build the dashboard bundle (PureScript, then Vite)
dashboard:
    spago build -p sp-dashboard
    vite build --config apps/dashboard/vite.config.js

# Serve the read API and dashboard, e.g. `just serve --db runs.db`
serve *args: dashboard
    spago run -p sp-server -- {{args}}

# Playwright smoke test against a freshly seeded Run Store
dashboard-test: dashboard
    cd apps/dashboard && playwright test

clean:
    rm -rf output .spago dist
