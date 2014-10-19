#!/bin/sh

# Tray
trayer &

# X stuff
xscreensaver -no-splash &

# Background
feh --bg-scale "$HOME/Pictures/backgrounds/haskell.png"

# Emacs
emacs --daemon
touch ~/xinit.ran

exec xmonad
