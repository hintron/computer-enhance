#!/bin/bash

PROJECT_DIR="$(git rev-parse --show-toplevel)"
OUT_DIR="$PROJECT_DIR/files/asm"

mkdir -p $OUT_DIR

# Install cargo (show) asm if haven't already.
# See https://github.com/pacak/cargo-show-asm
# NOTE: The cargo-asm crate is unmaintained and fails on Linux.
if ! cargo install --list | grep cargo-asm > /dev/null; then
    echo "Installing cargo asm command..."
    cargo install cargo-show-asm
fi

cargo asm \
--rust \
--intel \
"computer_enhance::decode::decode" 0 \
2>/dev/null > "$OUT_DIR/decode.asm"

cargo asm \
--color \
--rust \
--intel \
"computer_enhance::decode::decode" 0 | less

echo "Viewed $OUT_DIR/decode.asm"
