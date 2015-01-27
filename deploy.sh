#!/bin/sh

# setup dot files
ARCHIVE="/tmp/dot-files.tar.gz"
git archive -o "$ARCHIVE" HEAD

pushd "$HOME"
tar xvf "$ARCHIVE"
rm "$ARCHIVE"

EMACS_DOT_D="$HOME/.emacs.d"
mv "$EMACS_DOT_D" "${EMACS_DOT_D}.bak"
git clone https://github.com/netsight/emacs.d "$EMACS_DOT_D"
cd "$EMACS_DOT_D"
make


