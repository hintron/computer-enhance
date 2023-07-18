#!/bin/bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
PROJECT_DIR=$(realpath "$SCRIPT_DIR/..")
FILE_DIR="$PROJECT_DIR/files"
ASM_BUILD_DIR="$FILE_DIR/build-decode-regress"
SRC_DIR="$PROJECT_DIR/src"
TARGET_DIR="$PROJECT_DIR/target"
BIN="$PROJECT_DIR/target/debug/computer-enhance"

CHECK_RTOS="false"
if [ "$1" == "rtos" ]; then
    CHECK_RTOS="true"
fi

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
    BASE=$(basename $file)
    if ! $BIN "$file" "$ASM_BUILD_DIR/$BASE-tmp.asm" > "$ASM_BUILD_DIR/$BASE-tmp.log"; then
        echo "ERROR: Decode program failed for $file"
        rc=1
        break
    fi
    nasm "$ASM_BUILD_DIR/$BASE-tmp.asm" -o "$ASM_BUILD_DIR/$BASE-tmp.o"
    if ! diff "$ASM_BUILD_DIR/$BASE-tmp.o" "$file" ; then
        echo "ERROR: Assembly of decoded output failed for $file"
        rc=1
        break
    fi
    if ! diff "$ASM_BUILD_DIR/$BASE-tmp.o" "$file" ; then
        echo "ERROR: Decoded output didn't match golden for $file"
        rc=1
        break
    fi
done

# TODO: Assemble individual files, not final rtos file
# TODO: In handlers_simptris.s, there is:
    # L_handlers_simptris_5:
    # 	DB	".",0
# This looks like an 0x2E, which is an ES segment overide prefix (not sure what that does)
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
    RTOS_ASM="$RTOS_DIR/artossfinal.s"
    RTOS_BIN="$RTOS_DIR/artoss.bin"
    RTOS_BIN_TRUNC="$ASM_BUILD_DIR/artoss.bin.truncated"
    cd $RTOS_DIR
    make clean
    if ! make; then
        echo "ERROR: Failed to build RTOS"
        rc=1
        break
    fi
    cd $FILE_DIR

    # https://unix.stackexchange.com/questions/13907/delete-the-first-n-bytes-of-files
    echo "Stripping off the first 100 data bytes $RTOS_BIN..."
    tail +257c $RTOS_BIN > $RTOS_BIN_TRUNC

    echo "Checking decode of $RTOS_BIN_TRUNC..."
    BASE=$(basename $RTOS_BIN_TRUNC)
    if ! $BIN "$RTOS_BIN_TRUNC" "$ASM_BUILD_DIR/$BASE-tmp.asm" > "$ASM_BUILD_DIR/$BASE-tmp.log"; then
        echo "ERROR: Decode program failed for $RTOS_BIN_TRUNC"
        rc=1
        break
    fi
    nasm "$ASM_BUILD_DIR/$BASE-tmp.asm" -o "$ASM_BUILD_DIR/$BASE-tmp.o"
    if ! diff "$ASM_BUILD_DIR/$BASE-tmp.o" "$file" ; then
        echo "ERROR: Assembly of decoded output failed for $file"
        rc=1
        break
    fi
    if ! diff "$ASM_BUILD_DIR/$BASE-tmp.o" "$file" ; then
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
