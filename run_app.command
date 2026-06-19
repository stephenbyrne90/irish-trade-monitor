#!/bin/zsh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

echo "Starting Irish Trade Monitor from:"
echo "  $SCRIPT_DIR"
echo

R -e 'shiny::runApp("app", launch.browser = TRUE)'
