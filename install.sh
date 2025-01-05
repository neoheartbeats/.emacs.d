#!/usr/bin/env zsh

# Exit immediately on error (-e), treat unset variables as errors (-u), and fail on any
# error within piped commands (pipefail). Adjust flags according to your preference.
set -euo pipefail

# 1. Initialize and update submodules recursively
git submodule update --init --recursive

# 2. Build Org mode: use make’s -C flag to avoid manual cd/subshell
make -C ~/.emacs.d/site-lisp/org/ autoloads
make -C ~/.emacs.d/site-lisp/org/

# 3. Run Emacs batch commands to refresh, upgrade packages, and install Treesit grammars
emacs --batch \
      --load ~/.emacs.d/init.el \
      --eval "(progn
                (require 'package)
                (package-initialize)
                (package-refresh-contents)
                (when (fboundp 'package-upgrade-all)
                  (package-upgrade-all))
                (require 'treesit-auto)
                (treesit-auto-install-all))"

echo "install.sh: Installation completed!"
