#!/bin/bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
BUILD_DIR="$SCRIPT_DIR/build"

cd "$SCRIPT_DIR"
mkdir -p "$BUILD_DIR"
# Make each asm file with nasm
for file in "$SCRIPT_DIR"/*.asm; do
    if [ -f "$file" ]; then
        new_name=$(basename "${file%.*}")
        echo "Assembling $new_name"
        nasm "$file" -o "$BUILD_DIR/$new_name"
    fi
done
