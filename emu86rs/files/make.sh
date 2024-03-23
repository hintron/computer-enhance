#!/bin/bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
BIN_86_DIR="$SCRIPT_DIR/bin86"
BUILD_DIR="$SCRIPT_DIR/build"
MISC_DIR="$SCRIPT_DIR/misc"
SRC_DIR_DECODE="$SCRIPT_DIR/decode-regress"
SRC_DIR_SNAKE="$SCRIPT_DIR/snake-regress"
if [ -d "$HOME/code/425_artoss/" ]; then
    SRC_DIR_RTOS="$HOME/code/425_artoss/"
elif [ -d "$HOME/code/artoss/" ]; then
    SRC_DIR_RTOS="$HOME/code/artoss/"
else
    echo "WARNING: Unable to find RTOS dir! Can't build RTOS"
    SRC_DIR_RTOS=""
fi
SRC_DIR_SIMULATE="$SCRIPT_DIR/simulate-regress"
SRC_DIR_SIMULATE_IP="$SCRIPT_DIR/simulate-ip-regress"
SRC_DIR_SIMULATE_CYCLES="$SCRIPT_DIR/simulate-ip-cycles-regress"
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
mkdir -p "$BIN_86_DIR"
mkdir -p "$BUILD_DIR"

# Build each decode asm file with nasm
for file in "$SRC_DIR_DECODE"/*.asm; do
    if [ -f "$file" ]; then
        new_name=$(basename "${file%.*}")
        echo "Assembling (decode) $new_name"
        if ! nasm "$file" -o "$BIN_86_DIR/$new_name" -l "$BUILD_DIR/${new_name}.lst"; then
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
        if ! nasm "$file" -o "$BIN_86_DIR/$new_name" -l "$MISC_DIR/${new_name}.lst" ; then
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
        if ! nasm "$file" -o "$BIN_86_DIR/$new_name" -l "$BUILD_DIR/${new_name}.lst"; then
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
        if ! nasm "$file" -o "$BIN_86_DIR/$new_name" -l "$BUILD_DIR/${new_name}.lst"; then
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
        if ! nasm "$file" -o "$BIN_86_DIR/$new_name" -l "$BUILD_DIR/${new_name}.lst"; then
            echo "ERROR: nasm failed!"
            exit 1
        fi
    fi
done

for file in "$SRC_DIR_SIMULATE_8086"/*.asm; do
    if [ -f "$file" ]; then
        new_name=$(basename "${file%.*}")
        echo "Assembling (simulate w/ IP and 8086 cycle estimates) $new_name"
        if ! nasm "$file" -o "$BIN_86_DIR/$new_name" -l "$BUILD_DIR/${new_name}.lst"; then
            echo "ERROR: nasm failed!"
            exit 1
        fi
    fi
done

# Build RTOS, if possible
if [ "$SRC_DIR_RTOS" != "" ]; then
    RTOS_DIR="$SRC_DIR_RTOS/labs/lab8"
    RTOS_BIN_ORIG="$RTOS_DIR/artoss.bin"
    RTOS_BIN="$BIN_86_DIR/artoss.bin"

    echo "Assembling RTOS..."

    cd "$RTOS_DIR" || exit
    make clean > /dev/null
    if ! make > /dev/null; then
        echo "ERROR: Failed to build RTOS"
        exit 1
    fi
    cd "$SCRIPT_DIR" || exit

    if [ ! -f "$RTOS_BIN_ORIG" ]; then
        echo "ERROR: Could not find RTOS binary file '$RTOS_BIN_ORIG'"
        exit 1
    fi
    cp "$RTOS_BIN_ORIG" "$RTOS_BIN"
fi
