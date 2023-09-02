#!/bin/bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
BUILD_DIR_DECODE="$SCRIPT_DIR/build-decode-regress"
BUILD_DIR_SIMULATE="$SCRIPT_DIR/build-simulate-regress"

rm -rf "$BUILD_DIR_DECODE"
rm -rf "$BUILD_DIR_SIMULATE"
