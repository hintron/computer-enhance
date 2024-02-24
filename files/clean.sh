#!/bin/bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
BUILD_DIR_DECODE="$SCRIPT_DIR/build-decode-regress"
BUILD_DIR_SIMULATE="$SCRIPT_DIR/build-simulate-regress"
BUILD_DIR_SIMULATE_IP="$SCRIPT_DIR/build-simulate-ip-regress"
BUILD_DIR_SIMULATE_CYCLES="$SCRIPT_DIR/build-simulate-ip-cycles-regress"
BUILD_DIR_8086="$SCRIPT_DIR/build-simulate-8086-regress"

rm -rf "$BUILD_DIR_DECODE"
rm -rf "$BUILD_DIR_SIMULATE"
rm -rf "$BUILD_DIR_SIMULATE_IP"
rm -rf "$BUILD_DIR_SIMULATE_CYCLES"
rm -rf "$BUILD_DIR_8086"
