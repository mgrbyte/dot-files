source ~/.profile

USER_BASH_COMPLETION="$HOME/.bash_completion"

PS1='\[\033[0;33m\]\u\[\033[00m\] in \[\033[0;36m\]$( pwd ) on \[\033[0;35m\]\h\[\033[00m\] \n\[\033[1;32m\]\$\[\033[;m\] '

if [ -n "$VIRTUAL_ENV" ]; then
    PS1="\[\033[01;34m\](\$(basename '$VIRTUAL_ENV'))\e[0m$PS1"
fi

DIRCOLORS_FILE="$HOME/.dircolors"
if [ -f "$DIRCOLORS_FILE" ]; then
    if [ "$(uname)" == "Darwin" ]; then
        DIRCOLORS="/usr/local/bin/gdircolors"
    else
        DIRCOLORS="dircolors"
    fi
    eval "$($DIRCOLORS $DIRCOLORS_FILE)"
    export DIRCOLORS 
fi

export PS1
