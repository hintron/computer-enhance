#!/bin/bash

# Instructions: run install-pre-commit.sh to install this Git pre-commit script.

# Redirect all output to stderr
exec 1>&2
# Run rustfmt on the project
if ! cargo fmt -- --check > /dev/null; then
    echo "Commit ABORTED: Please run \`cargo fmt\`."
    echo "Output of \`cargo fmt -- --check\`:"
    # Run `cargo fmt -- --check` to see what rustfmt would change
    cargo fmt -- --check
    exit 1
fi

# Build the project and build the benchmarks
echo "Building source code..."
if ! cargo build -q; then
    echo "Commit ABORTED: \`cargo build\` failed."
    exit 1
fi
if ! cargo build --bench benchmarks -q; then
echo "Building benchmarks..."
    echo "Commit ABORTED: \`cargo build --bench benchmarks\` failed."
    exit 1
fi

echo "Running 'cargo test'..."
# Run all rust tests to make sure things succeed
if ! cargo test -q > /dev/null; then
    echo "Commit ABORTED: \`cargo test\` failed."
    exit 1
fi

# Only run regressions if code changed in emu86rs/
if [[ $(git diff --staged --name-only | grep "^emu86rs/") ]]; then
    echo "Running emu86rs regressions..."
    # Run all rust tests to make sure things succeed
    if ! ./emu86rs/scripts/regress.sh >& regress.log; then
        REPO=$(git rev-parse --show-toplevel)
        echo "Commit ABORTED: Emulator: \`regress.sh\` failed."
        echo "Please see $REPO/regress.log or rerun regressions with $REPO/emu86rs/scripts/regress.sh"
        exit 1
    fi
    rm regress.log
fi
