#!/usr/bin/env zsh

# Exit on error (-e), treat unset variables as errors (-u), 
# and fail on any error within piped commands (-o pipefail).
# Add or remove flags based on your preference.
set -euo pipefail

# Update all submodules recursively
git submodule update --init --recursive

# Build org-mode in a subshell so we don't have to manually 'cd' back
(
  cd ~/.emacs.d/site-lisp/org/
  make autoloads
  make
)

echo "Installation completed."
