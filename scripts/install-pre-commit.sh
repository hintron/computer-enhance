#!/bin/bash

# Get the location of the folder containing .git
PROJECT_DIR="$(git rev-parse --show-toplevel)"
# Set scripts folder
SCRIPT_DIR="$PROJECT_DIR/scripts"
# Set .git folder
GIT_DIR="$PROJECT_DIR/.git"

# Install the pre-commit hook
set -x
cp "$SCRIPT_DIR/git-pre-commit.sh" "$GIT_DIR/hooks/pre-commit"
chmod 755 "$GIT_DIR/hooks/pre-commit"
set +x
