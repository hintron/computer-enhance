#!/bin/bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
BUILD_DIR_DECODE="$SCRIPT_DIR/build-decode-regress"
SRC_DIR_DECODE="$SCRIPT_DIR/decode-regress"
BUILD_DIR_SIMULATE="$SCRIPT_DIR/build-simulate-regress"
SRC_DIR_SIMULATE="$SCRIPT_DIR/simulate-regress"
BUILD_DIR_SIMULATE_IP="$SCRIPT_DIR/build-simulate-ip-regress"
SRC_DIR_SIMULATE_IP="$SCRIPT_DIR/simulate-ip-regress"

cd "$SCRIPT_DIR" || exit
mkdir -p "$BUILD_DIR_DECODE"
mkdir -p "$BUILD_DIR_SIMULATE"
mkdir -p "$BUILD_DIR_SIMULATE_IP"

# Build each decode asm file with nasm
for file in "$SRC_DIR_DECODE"/*.asm; do
    if [ -f "$file" ]; then
        new_name=$(basename "${file%.*}")
        echo "Assembling (decode) $new_name"
        if ! nasm "$file" -o "$BUILD_DIR_DECODE/$new_name"; then
            echo "ERROR: nasm failed!"
            exit 1
        fi
    fi
done

# Build each simulate asm file with nasm
for file in "$SRC_DIR_SIMULATE"/*.asm; do
    if [ -f "$file" ]; then
        new_name=$(basename "${file%.*}")
        echo "Assembling (simulate w/o IP) $new_name"
        if ! nasm "$file" -o "$BUILD_DIR_SIMULATE/$new_name"; then
            echo "ERROR: nasm failed!"
            exit 1
        fi
    fi
done

# Build each simulate-ip asm file with nasm
for file in "$SRC_DIR_SIMULATE_IP"/*.asm; do
    if [ -f "$file" ]; then
        new_name=$(basename "${file%.*}")
        echo "Assembling (simulate w/ IP) $new_name"
        if ! nasm "$file" -o "$BUILD_DIR_SIMULATE_IP/$new_name"; then
            echo "ERROR: nasm failed!"
            exit 1
        fi
    fi
done
