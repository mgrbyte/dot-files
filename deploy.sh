#!/bin/sh
ARCHIVE="/tmp/dot-files.tar.gz"
git archive -o "$ARCHIVE" HEAD
cd "$HOME"
tar tvf "$ARCHIVE"
rm "$ARCHIVE"

