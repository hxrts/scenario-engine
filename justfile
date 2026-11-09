export PATH := justfile_directory() + "/node_modules/.bin:" + env_var('PATH')

packages := "core laws schema checks world strategy assembly premises effects handlers adapters views experiments testkit flags cli server dashboard"

_default:
    @just --list

# Compile every package
build:
    spago build --strict

# Run every package's test suite
test: _premises
    #!/usr/bin/env bash
    set -euo pipefail
    for p in {{packages}}; do spago test -p sp-$p; done

# Test one package
[no-exit-message]
test-pkg pkg:
    @case " {{packages}} " in *" {{pkg}} "*) ;; *) echo "unknown package '{{pkg}}' (packages: {{packages}})"; exit 1 ;; esac
    @just _premises
    spago test -p sp-{{pkg}}

# Format all PureScript sources
fmt:
    purs-tidy format-in-place "packages/*/src/**/*.purs" "packages/*/test/**/*.purs" "apps/*/src/**/*.purs" "apps/*/test/**/*.purs"

# Check formatting without writing
_fmt-check:
    purs-tidy check "packages/*/src/**/*.purs" "packages/*/test/**/*.purs" "apps/*/src/**/*.purs" "apps/*/test/**/*.purs"

# Check package dependency direction
_deps:
    node scripts/check/deps.js

# Check the named layout rules (docs/07 §3)
_layout:
    node scripts/check/layout.js

# The three layers for impure work (docs/07 §1)
_impurity:
    node scripts/check/impurity.js --samples
    node scripts/check/impurity.js
    node scripts/check/impurity.js --compile

# Manual implementations of what should be derived (docs/07 §3)
_derived:
    node scripts/check/derived.js --samples
    node scripts/check/derived.js

# Compile the premise DSL to JSON, then generate the capstone recording (no model call)
_premises:
    spago run -p sp-cli --main Compile
    spago run -p sp-cli --main Transcript
    spago run -p sp-server --main OpenApi

# One Premise's OpenAPI documents: its references list the World's names (--server url, --out dir)
[positional-arguments]
openapi premise="meridian" *args:
    @just _premises
    p="$1"; shift; spago run -p sp-server --main OpenApi -- --premise "$p" "$@"

# fmt-check, deps, layout, derived, build, test
check: _fmt-check build _deps _layout _impurity _derived test _docs

# docs/05's marked tables against the compiled premise
_docs: _premises
    node scripts/check/docs.js --samples
    node scripts/check/docs.js

# check + dashboard smoke test
all: check dashboard-test

# Simulate a game
[positional-arguments]
sim *args:
    @just _premises
    spago run -p sp-cli -- run "$@"

# Live facilitator session
[positional-arguments]
session *args:
    @just _premises
    spago run -p sp-cli -- session "$@"


# shadcn/ui components (apps/dashboard/ui) and their theme (apps/dashboard/theme.css) are generated,
# never committed. Their checksums are the baseline in apps/dashboard/ui.sha256 (scripts/ui-checksums.sh).
#
# components.json: the `utils`, `lib` and `hooks` aliases name paths that nothing generates. The
# generated files import `cn` from the npm package `cn` (a dependency in package.json), not from
# @/lib/utils, and the only alias the output uses is `@/ui`. The aliases stay because the shadcn CLI
# expects them; they are not references to files in this repo.
ui_components := "theme-neutral card tabs checkbox button input chart"

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
      echo "apps/dashboard/ui does not match apps/dashboard/ui.sha256. If the upstream change is intended, run 'just ui-accept'. Otherwise run 'rm -rf apps/dashboard/ui' and 'just _ui'."
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
    # compiled modules only: output/ also holds the run stores and spago's own cache, which change on every run
    if [ -f apps/dashboard/dist/index.html ] && [ -z "$(find apps/dashboard/src apps/dashboard/ui apps/dashboard/app.css apps/dashboard/theme.css apps/dashboard/index.html apps/dashboard/main.js apps/dashboard/vite.config.mjs apps/dashboard/ports.mjs -newer apps/dashboard/dist/index.html -print -quit)" ] && [ -z "$(find output \( -name index.js -o -name foreign.js \) -newer apps/dashboard/dist/index.html -print -quit)" ]; then
      echo "dashboard bundle is up to date"
      exit 0
    fi
    vite build --config apps/dashboard/vite.config.mjs

# Serve the API and dashboard
[positional-arguments]
serve *args:
    @just _premises _dashboard
    @scripts/free-port.sh "$@"
    spago run -p sp-server -- "$@"

# A production build of one premise: only its compiled files, under build/premises/<id>, with no
# fixtures and no capstone. Serve it with SETTINGS_DIR=build/premises/<id> (docs/06 §8.1).
[positional-arguments]
build-prod premise: _ui _dashboard
    rm -rf build/premises/{{premise}}
    mkdir -p build/premises/{{premise}}
    SETTINGS_DIR=build/premises/{{premise}} spago run -p sp-cli --main Compile -- --only {{premise}}

# One-off, billed live check of the joint pass: one real model call (docs/06 §14). Not in any suite.
live-interpret:
    #!/usr/bin/env bash
    set -euo pipefail
    if [ "${ALLOW_BILLED_LIVE:-}" != "1" ]; then echo "refused: this makes a billed model call; set ALLOW_BILLED_LIVE=1"; exit 2; fi
    just _premises
    spago test -p sp-adapters -m Test.Adapters.LiveInterpret

# Dashboard smoke test (Playwright). Needs the browser binaries once: `cd apps/dashboard && npx playwright install`.
dashboard-test: _premises _dashboard
    cd apps/dashboard && playwright test

_clean:
    rm -rf output .spago dist apps/dashboard/dist apps/dashboard/ui apps/dashboard/theme.css
