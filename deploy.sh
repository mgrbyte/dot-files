#!/bin/sh

# setup dot files
ARCHIVE="/tmp/dot-files.tar.gz"
git archive -o "$ARCHIVE" HEAD
pushd "$HOME"
tar xvf "$ARCHIVE"
rm "$ARCHIVE"

# Ensure emacs is installed properly
cd "$HOME/.emacs.d"
git remote -v | grep "netsight/emacs"
if [ $? -ne 0 ]; then
    cd "$HOME"
    git clone https://github.com/netsight/emacs.d ~/.emacs.d
    curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
    cd ~/.emacs.d
    cask update
    cask upgrade
fi
popd

