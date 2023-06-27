#!/bin/bash

# Instructions: run install-pre-commit.sh to install this Git pre-commit script.

# Redirect all output to stderr
exec 1>&2
# Run rustfmt on the project
cargo fmt -- --check > /dev/null
if [ "$?" == "1" ]; then
    echo "Commit ABORTED: Please run \`cargo fmt\`."
    echo "Output of \`cargo fmt -- --check\`:"
    # Run `cargo fmt -- --check` to see what rustfmt would change
    cargo fmt -- --check
    exit 1
fi