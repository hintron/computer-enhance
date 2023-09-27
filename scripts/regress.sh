#!/bin/bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
PROJECT_DIR=$(realpath "$SCRIPT_DIR/..")
FILE_DIR="$PROJECT_DIR/files"
DECODE_BUILD_DIR="$FILE_DIR/build-decode-regress"
SIMULATE_BUILD_DIR="$FILE_DIR/build-simulate-regress"
SIMULATE_SRC_DIR="$FILE_DIR/simulate-regress"
BIN="$PROJECT_DIR/target/debug/computer-enhance"

CHECK_RTOS="false"
if [ "$1" == "rtos" ]; then
    CHECK_RTOS="true"
fi

DATE=$(date +"%Y-%m-%d at %H:%M:%S")
echo "Date: $DATE"

# Build everything in decode-regress and simulate-regress
cd "$FILE_DIR" || exit
"$FILE_DIR/clean.sh"
if ! "$FILE_DIR/make.sh"; then
    exit 1
fi

cd "$SCRIPT_DIR" || exit
if ! cargo build; then
    exit 1
fi

rc=0

# Check the decode of everything in decode-regress
for file in "$DECODE_BUILD_DIR"/*; do
    echo "Checking decode of $file..."
    BASE=$(basename "$file")
    if ! $BIN "$file" "$DECODE_BUILD_DIR/$BASE-tmp.asm" > "$DECODE_BUILD_DIR/$BASE-tmp.log"; then
        echo "ERROR: Decode program failed for $file"
        rc=1
        break
    fi
    nasm "$DECODE_BUILD_DIR/$BASE-tmp.asm" -o "$DECODE_BUILD_DIR/$BASE-tmp.o"
    if ! diff "$DECODE_BUILD_DIR/$BASE-tmp.o" "$file" -u; then
        echo "ERROR: Assembly of decoded output failed for $file"
        rc=1
        break
    fi
    if ! diff "$DECODE_BUILD_DIR/$BASE-tmp.o" "$file" -u; then
        echo "ERROR: Decoded output didn't match golden for $file"
        rc=1
        break
    fi
done

# TODO: Assemble individual files, not final rtos file
# TODO: In handlers_simptris.s, there is:
    # L_handlers_simptris_5:
    # 	DB	".",0
# This looks like an 0x2E, which is an ES segment override prefix (not sure what that does)
# So the problem is that my decoder can't know that this is data rather than an
# instruction. But NASM does when it assembles it. So perhaps this blind decode
# may not work well.
# How do real decoders handle data bytes?

# Use while loop to easily break out
while [ "$CHECK_RTOS" == "true" ]; do
    # Try decoding my ECEn 425 RTOS
    if [ -d "$HOME/code/425_artoss/" ]; then
        RTOS_REPO="$HOME/code/425_artoss/"
    elif [ -d "$HOME/code/artoss/" ]; then
        RTOS_REPO="$HOME/code/artoss/"
    else
        echo "ERROR: Unable to find RTOS dir!"
        rc=1
        break
    fi
    RTOS_DIR="$RTOS_REPO/labs/lab8"
    RTOS_BIN="$RTOS_DIR/artoss.bin"
    RTOS_BIN_TRUNC="$DECODE_BUILD_DIR/artoss.bin.truncated"
    cd "$RTOS_DIR" || exit
    make clean
    if ! make; then
        echo "ERROR: Failed to build RTOS"
        rc=1
        break
    fi
    cd "$FILE_DIR" || exit

    # https://unix.stackexchange.com/questions/13907/delete-the-first-n-bytes-of-files
    echo "Stripping off the first 100 data bytes $RTOS_BIN..."
    tail +257c "$RTOS_BIN" > "$RTOS_BIN_TRUNC"

    echo "Checking decode of $RTOS_BIN_TRUNC..."
    BASE=$(basename "$RTOS_BIN_TRUNC")
    if ! $BIN "$RTOS_BIN_TRUNC" "$DECODE_BUILD_DIR/$BASE-tmp.asm" > "$DECODE_BUILD_DIR/$BASE-tmp.log"; then
        echo "ERROR: Decode program failed for $RTOS_BIN_TRUNC"
        rc=1
        break
    fi
    nasm "$DECODE_BUILD_DIR/$BASE-tmp.asm" -o "$DECODE_BUILD_DIR/$BASE-tmp.o"
    if ! diff "$DECODE_BUILD_DIR/$BASE-tmp.o" "$file" -u; then
        echo "ERROR: Assembly of decoded output failed for $file"
        rc=1
        break
    fi
    if ! diff "$DECODE_BUILD_DIR/$BASE-tmp.o" "$file" -u; then
        echo "ERROR: Decoded output didn't match golden for $file"
        rc=1
        break
    fi
done


# Check the decode and simulation of everything in simulate-regress
for file in "$SIMULATE_BUILD_DIR"/*; do
    # If the glob found nothing, it will be treated as a file, so skip it 
    if [ ! -e "$file" ]; then continue; fi
    echo "Checking simulation of $file..."
    BASE=$(basename "$file")
    SIMULATE_OUTPUT="$SIMULATE_BUILD_DIR/$BASE-simulate.txt"
    SIMULATE_LOG="$SIMULATE_BUILD_DIR/$BASE-simulate.log"
    if ! $BIN "$file" "$SIMULATE_OUTPUT" --exec > "$SIMULATE_LOG"; then
        echo "ERROR: Simulation of program failed for $file"
        rc=1
        break
    fi
    SIMULATE_GOLDEN_OUTPUT="$SIMULATE_SRC_DIR/$BASE.txt"
    if ! diff "$SIMULATE_GOLDEN_OUTPUT" "$SIMULATE_OUTPUT" -u; then
        echo "ERROR: Simulation output didn't match golden output"
        echo "$SIMULATE_OUTPUT != $SIMULATE_GOLDEN_OUTPUT"
        rc=1
        break
    fi
done


if [ "$rc" == "0" ]; then
    echo "All regressions passed"
else
    echo "ERROR: Regressions failed"
fi
exit $rc
