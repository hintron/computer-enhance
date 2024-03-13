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
if !cargo build -q; then
    echo "Commit ABORTED: \`cargo build\` failed."
    exit 1
fi
if !cargo build --bench benchmarks -q; then
    echo "Commit ABORTED: \`cargo build --bench benchmarks\` failed."
    exit 1
fi

# Run all rust tests to make sure things succeed
if ! cargo test -q; then
    echo "Commit ABORTED: \`cargo test\` failed."
    exit 1
fi

# Run all rust tests to make sure things succeed
if ! ./emu86rs/scripts/regress.sh; then
    echo "Commit ABORTED: Emulator: \`regress.sh\` failed."
    exit 1
fi
