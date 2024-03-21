#!/bin/bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
BUILD_DIR="$SCRIPT_DIR/build"

echo "Cleaning build dir '$BUILD_DIR'..."
rm -rf "$BUILD_DIR"
