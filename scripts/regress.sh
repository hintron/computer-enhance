#!/bin/bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
PROJECT_DIR=$(realpath "$SCRIPT_DIR/..")
FILE_DIR="$PROJECT_DIR/files"
ASM_BUILD_DIR="$FILE_DIR/build"
SRC_DIR="$PROJECT_DIR/src"
TARGET_DIR="$PROJECT_DIR/target"
BIN="$PROJECT_DIR/target/debug/computer-enhance"

DATE=$(date +"%Y-%m-%d at %H:%M:%S")
echo "Date: $DATE"

cd $FILE_DIR
$FILE_DIR/clean.sh
if ! $FILE_DIR/make.sh; then
    exit 1
fi

cd $SCRIPT_DIR
if ! cargo build; then
    exit 1
fi

rc=0
for file in "$ASM_BUILD_DIR"/*; do
    echo "Checking decode of $file..."
    if ! RUST_BACKTRACE=1 $BIN "$file" > "$ASM_BUILD_DIR/tmp.asm" ; then
        echo "ERROR: Decode program failed for $file"
        rc=1
        break
    fi
    nasm "$ASM_BUILD_DIR/tmp.asm" -o "$ASM_BUILD_DIR/tmp.o"
    if ! diff "$ASM_BUILD_DIR/tmp.o" "$file" ; then
        echo "ERROR: Assembly of decoded output failed for $file"
        rc=1
        break
    fi
    if ! diff "$ASM_BUILD_DIR/tmp.o" "$file" ; then
        echo "ERROR: Decoded output didn't match golden for $file"
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
