#!/bin/bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
BUILD_DIR_DECODE="$SCRIPT_DIR/build-decode-regress"
SRC_DIR_DECODE="$SCRIPT_DIR/decode-regress"
BUILD_DIR_SNAKE="$SCRIPT_DIR/build-snake-regress"
SRC_DIR_SNAKE="$SCRIPT_DIR/snake-regress"
BUILD_DIR_SIMULATE="$SCRIPT_DIR/build-simulate-regress"
SRC_DIR_SIMULATE="$SCRIPT_DIR/simulate-regress"
BUILD_DIR_SIMULATE_IP="$SCRIPT_DIR/build-simulate-ip-regress"
SRC_DIR_SIMULATE_IP="$SCRIPT_DIR/simulate-ip-regress"
BUILD_DIR_SIMULATE_CYCLES="$SCRIPT_DIR/build-simulate-ip-cycles-regress"
SRC_DIR_SIMULATE_CYCLES="$SCRIPT_DIR/simulate-ip-cycles-regress"
BUILD_DIR_SIMULATE_8086="$SCRIPT_DIR/build-simulate-8086-regress"
SRC_DIR_SIMULATE_8086="$SCRIPT_DIR/simulate-8086-regress"

# See if nasm exists on the system
HAS_NASM="false"
command -v nasm &> /dev/null
if [ $? == 0 ]; then
    nasm -v &> /dev/null
    if [ $? == 0 ]; then
        HAS_NASM="true"
    fi
fi

if [ "$HAS_NASM" != true ]; then
    echo "NASM wasn't found... Can't build assembly files"
    exit 1
fi

cd "$SCRIPT_DIR" || exit
mkdir -p "$BUILD_DIR_DECODE"
mkdir -p "$BUILD_DIR_SNAKE"
mkdir -p "$BUILD_DIR_SIMULATE"
mkdir -p "$BUILD_DIR_SIMULATE_IP"
mkdir -p "$BUILD_DIR_SIMULATE_CYCLES"
mkdir -p "$BUILD_DIR_SIMULATE_8086"

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

# Build each Snake asm file with nasm
for file in "$SRC_DIR_SNAKE"/*.asm; do
    if [ -f "$file" ]; then
        new_name=$(basename "${file%.*}")
        echo "Assembling (snake) $new_name"
        if ! nasm "$file" -o "$BUILD_DIR_SNAKE/$new_name"; then
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

# Build each simulate-ip-cycles asm file with nasm
for file in "$SRC_DIR_SIMULATE_CYCLES"/*.asm; do
    if [ -f "$file" ]; then
        new_name=$(basename "${file%.*}")
        echo "Assembling (simulate w/ IP and cycle estimates) $new_name"
        if ! nasm "$file" -o "$BUILD_DIR_SIMULATE_CYCLES/$new_name"; then
            echo "ERROR: nasm failed!"
            exit 1
        fi
    fi
done

for file in "$SRC_DIR_SIMULATE_8086"/*.asm; do
    if [ -f "$file" ]; then
        new_name=$(basename "${file%.*}")
        echo "Assembling (simulate w/ IP and 8086 cycle estimates) $new_name"
        if ! nasm "$file" -o "$BUILD_DIR_SIMULATE_8086/$new_name"; then
            echo "ERROR: nasm failed!"
            exit 1
        fi
    fi
done
