#!/bin/sh

# setup dot files
ARCHIVE="/tmp/dot-files.tar.gz"
git archive -o "$ARCHIVE" HEAD
cd "$HOME"
tar xvf "$ARCHIVE"
rm "$ARCHIVE"

# Ensure emacs is installed properly
cd "$HOME/.emacs.d"
git remote -v | grep "netsight/emacs"
cd "$HOME"
if [ $? -ne 0 ]; then
    cd "$HOME"
    git clone https://github.com/netsight/emacs.d ~/.emacs.d
    curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
    cd ~/.emacs.d
    cask update
    cask upgrade
fi

