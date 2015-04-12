export EDITOR="et"
export EMACS_SERVER_FILE="$HOME/.emacs.d/server/server"
export EMAIL="mattr@netsight.co.uk"
export GIT_TEMPLATES_DIR="$HOME/.git-templates"
export GREP_COLOR="33;51;1"
export GREP_OPTIONS="--color=auto"
export LANGUAGE="en_GB:en"
export LC_COLLATE="en_GB.utf8"
export LC_CTYPE="en_GB.utf8"
export LC_MESSAGES="en_GB.utf8"
export PIP_DOWNLOAD_CACHE="$HOME/.pip/download-cache"
export VENV_WRAPPER="$HOME/.local/bin/virtualenvwrapper.sh"
export VISUAL="$EDITOR -nw -q"
export WORKON_HOME="$HOME/.virtualenvs"

# Make sure /usr/local is before /usr/bin so custom stuff gets preference
export PATH="/usr/local/bin:$(echo $PATH | sed 's/\/usr\/local\/bin://g')"

# Anaconda
ANACONDA3_HOME="$HOME/anaconda3"
if [ -d "$ANACONDA3_HOME" ]; then
    export PATH="$HOME/anaconda3/bin:$PATH"
fi

CASK="$HOME/.cask"
if [ -e "$CASK" ]; then
    export PATH="$PATH:$CASK/bin"
fi

DOT_LOCAL="$HOME/.local"
if [ -e "$DOT_LOCAL" ]; then
    export PATH="$PATH:$DOT_LOCAL/bin"
fi

NIXOS="$HOME/.nix-profile"
if [ -e "$NIXOS" ]; then
    export PATH="$PATH:$NIXOS/bin"
fi

if [ -d "$HOME/bin" ] ; then
    PATH="$PATH:$HOME/bin"
fi

LOCAL="$HOME/.local"
if [ -d "$LOCAL/bin" ]; then
    PATH="$LOCAL/bin:$PATH"
fi

if [ -x $(which pew-new) ]; then
    export PROJECT_HOME="$HOME/git"
elif [ -f "$VENV_WRAPPER" ]; then
    source "$VENV_WRAPPER"
else
    echo "No Python virtualenv support (pew nor virtualenvwrapper)"
fi

alias ls="ls -p"
alias ll="ls -l"
alias la="ls -a"
alias ec="emacsclient -c"
alias et="emacsclient -t"
alias pbcopy="xclip -selection clipboard";
alias pbpaste="xclip -selection clipboard -o";
