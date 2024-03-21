#!/bin/bash

# Rebuild everything in decode-regress and simulate-regress
# This is only possible on a machine with NASM already installed.

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
PROJECT_DIR=$(realpath "$SCRIPT_DIR/../..")
FILE_DIR="$PROJECT_DIR/emu86rs/files"

cd "$FILE_DIR" || exit
"$FILE_DIR/clean.sh"
if ! "$FILE_DIR/make.sh"; then
    exit 1
fi
