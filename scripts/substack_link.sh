#!/bin/bash

# Wrapper of link-generator.sh for substack URLs.
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
"$SCRIPT_DIR/link-generator.sh" "substack" "$@"
