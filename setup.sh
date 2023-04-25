#!/usr/bin/env sh

[ ! -d ~/.emacs.d/site-lisp/ ] && exit 1

cd ~/.emacs.d/site-lisp/

git clone https://git.tecosaur.net/tec/org-mode.git

if [ "$?"-ne 0]; then
    echo "Failed."
    exit 1
fi

cd ~/.emacs.d/site-lisp/org-mode/ && make autoloads

mv ~/.emacs.d/site-lisp/org-mode/lisp/ ~/.emacs.d/site-lisp/org-lisp/
rm -rf ~/.emacs.d/site-lisp/org-mode/

echo "DONE.\n I am going to create following dirs to ensure this working:"
echo "1. \"~/Developer/\"\n2. \"~/Shelter/\""
echo "Press \"y(n)\" to give me the permission: "

read args

[[ -z "${args}" ]] && args="y"
if [ "$args" = "y" ]; then
    [ -d ~/Developer/ ] || mkdir ~/Developer/
    [ -d ~/Shelter/ ] || mkdir ~/Shelter/ && \
            mkdir ~/Shelter/dates/ \
                  ~/Shelter/notes/ \
                  ~/Shelter/repos/
fi

echo "Please following the final steps before starting your Emacs:"
echo "1. Install the font \"Pes Mono\" from \"https://github.com/ilyaw39/PesMono/\"."
echo "2. Install the font SF Pro from Apple (for display for icons)."
echo "3. Ensure \"ripgrep\" and \"sqlite3\" is in your PATH."
echo "4. Make sure TeX Live 2023 is correctly installed on your system."
