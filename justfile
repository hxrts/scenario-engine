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

# Ensure apps/dashboard/ui matches ui.sha256 (contents and file list). Generates ui/ when absent. A
# changed, extra or missing file fails rather than being silently regenerated.
ui:
    #!/usr/bin/env bash
    set -euo pipefail
    if [ ! -d apps/dashboard/ui ]; then just _ui-generate; fi
    if ! scripts/ui-checksums.sh check; then
      echo "apps/dashboard/ui does not match apps/dashboard/ui.sha256. If the upstream change is intended, run 'just ui-accept'. Otherwise run 'rm -rf apps/dashboard/ui' and 'just ui'."
      exit 1
    fi

# Regenerate ui/ from the pinned CLI and record its checksums as the new baseline
ui-accept:
    just _ui-generate
    scripts/ui-checksums.sh record

# Build the dashboard bundle (PureScript, then Vite)
dashboard: ui
    spago build -p sp-dashboard
    vite build --config apps/dashboard/vite.config.js

# Serve the read API and dashboard, e.g. `just serve --db runs.db`
serve *args: scenarios dashboard
    spago run -p sp-server -- {{args}}

# Playwright smoke test against a freshly seeded Run Store. Needs the browser binaries once: `npx playwright install`.
dashboard-test: scenarios dashboard
    cd apps/dashboard && playwright test

clean:
    rm -rf output .spago dist apps/dashboard/dist apps/dashboard/ui apps/dashboard/theme.css
