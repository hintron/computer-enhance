#!/bin/bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
FILE_DIR="$SCRIPT_DIR/../files"
SRC_DIR="$SCRIPT_DIR/../src"
TARGET_DIR="$SCRIPT_DIR/../target"
BIN=$TARGET_DIR/debug/computer-enhance

DATE=$(date +"%Y-%m-%d at %H:%M:%S")
echo "Date: $DATE"

cd $FILE_DIR
$FILE_DIR/clean.sh
$FILE_DIR/make.sh

cd $SCRIPT_DIR
cargo build

rc=0
$BIN $FILE_DIR/build/listing_0037_single_register_mov > tmp37.asm
nasm tmp37.asm -o tmp37.o
if ! diff tmp37.o $FILE_DIR/build/listing_0037_single_register_mov ; then
    echo "ERROR: Listing 37 regression failed"
    rc=1
fi
rm tmp37.asm
rm tmp37.o

$BIN $FILE_DIR/build/listing_0038_many_register_mov > tmp38.asm
nasm tmp38.asm -o tmp38.o
if ! diff tmp38.o $FILE_DIR/build/listing_0038_many_register_mov ; then
    echo "ERROR: Listing 38 regression failed"
    rc=1
fi
rm tmp38.asm
rm tmp38.o

if [ "$rc" == "0" ]; then
    echo "All regressions passed"
else
    echo "ERROR: One or more failures"
fi
exit $rc
