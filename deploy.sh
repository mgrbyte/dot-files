#!/bin/zsh
stow -R -v $(find . -maxdepth 1 -type d -exec basename {} \; | grep -vE '^\.')


