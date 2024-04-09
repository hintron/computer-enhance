#!/bin/bash

# Wrapper of link-generator.sh for youtube URLs.
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
"$SCRIPT_DIR/link-generator.sh" "youtube" "$@"
