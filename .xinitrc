#!/bin/sh

setxkbmap -layout gb -option ctrl:nocaps

# Tray
pgrep trayer
[ $? -eq 0 ] || trayer &

# X stuff
pgrep xscreensaver 
[ $? -eq 0 ] || xscreensaver -no-splash &

# Background
feh --bg-scale "$HOME/Pictures/backgrounds/haskell.png"

# Emacs
pgrep -f "emacs --daemon"
[ $? -eq 0 ] || emacs --daemon

pgrep xmonad || exec xmonad
