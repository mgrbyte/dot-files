#!/bin/sh

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

LOCAL="$HOME/.local"
if [ -d "$LOCAL/bin" ]; then
	PATH="$LOCAL/bin:$PATH"
fi

USER_BASH_COMPLETION="$HOME/.bash_completion"

DIRCOLORS_FILE="$HOME/.dircolors"
if [ -f "$DIRCOLORS_FILE" ]; then
    if [ "$(uname)" == "Darwin" ]; then
        DIRCOLORS="/usr/local/bin/gdircolors"
    else
        DIRCOLORS="dircolors"
    fi
    eval "$($DIRCOLORS $DIRCOLORS_FILE)"
fi

# Prompt custom-ly
export PS1='\[\033[0;33m\]\u\[\033[00m\] in \[\033[0;36m\]$( pwd ) on \[\033[0;35m\]\h\[\033[00m\] \n\[\033[1;32m\]\$\[\033[;m\] '

# Emacsen
export EMACS_SERVER_FILE="$HOME/.emacs.d/server/server"
export EDITOR="emacs -nw"
export VISUAL="$EDITOR -nw -q"

if [ "$(uname)" == "Darwin" ]; then
    alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs"
    alias emacsclient="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -f $EMACSSERVER_FILE"    
fi

export PIP_DOWNLOAD_CACHE="$HOME/.pip/download-cache"
export LANGUAGE="en_GB:en"
export LC_MESSAGES="en_GB.utf8"
export LC_CTYPE="en_GB.utf8"
export LC_COLLATE="en_GB.utf8"
export GREP_COLOR="33;51;1"
export GREP_OPTIONS="--color=auto"


# Make sure /usr/local is before /usr/bin so custom stuff gets preference
export PATH="/usr/local/bin:$(echo $PATH | sed 's/\/usr\/local\/bin://g')"
if [ "$(uname)" == "Darwin" ]; then
    export PATH="$PATH:/Applications/Emacs.app/Contents/MacOS/bin"
fi

CASK="$HOME/.cask"
if [ -e "$CASK" ]; then
    export PATH="$CASK/bin:$PATH"
fi

DOT_LOCAL="$HOME/.local"
if [ -e "$DOT_LOCAL" ]; then
    export PATH="$DOT_LOCAL/bin:$PATH"
fi

NIXOS="$HOME/.nix-profile"
if [ -e "$NIXOS" ]; then
    export PATH="$NIXOS/bin:$PATH"
fi

which setxkbmap 2> /dev/null
if [ $? -eq 0 ]; then
    setxkbmap -layout us -option ctrl:nocaps -geometry hhk
fi

alias ls="ls -p"
alias ll="ls -l"
alias la="ls -a"
alias pbcopy="xclip -selection clipboard";
alias pbpaste="xclip -selection clipboard -o";
alias ipython="ipython --profile=$USER"
