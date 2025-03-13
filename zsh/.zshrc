HOSTNAME="$(hostname)"

# Hint: Disable advanced theme for emacs tramp compat by settng ZSH_THEME="".
if [ $TERM = "dumb" ]; then
    unsetopt zle
    PS1='$ '
    ZSH_THEME=""
else
    ZSH_THEME="socrates"
fi
export ZSH_THEME

plugins=(git python themes)


alias cljsbuild="lein trampoline cljsbuild $@"
alias ls="ls --group-directories-first -p"
alias ll="ls -g"
alias la="ls -a"
alias lt="ls -lt"
alias grep="grep --color=auto"
alias egrep="egrep --color=auto"
alias rgrep="rgrep --color=auto"
alias pbcopy="xclip -selection clipboard";
alias pbpaste="xclip -selection clipboard -o";

alias psj="ps awx | grep 'jav[a]'"
alias rgrep-clj='grep --include="*.clj" -r'
alias rgrep-py='grep -r --include="*.py"'

source "$ZSH/oh-my-zsh.sh"

DISPLAY=""
which wsl.exe &> /dev/null
if [ $? -eq 0  ] && [ -z "$DISPLAY" ]; then
    DISPLAY="$(grep -m 1 nameserver /etc/resolv.conf | awk '{print $2}')"
fi

if [ ! -z "$DISPLAY" ]; then
    export DISPLAY="$DISPLAY:0"
    setxkbmap -layout us -option ctrl:nocaps
else
    echo "X is not running!"
fi

export NVM_DIR="$HOME/.nvm"
if [ -d ${NVM_DIR} ]; then
    [ -s "${NVM_DIR}/nvm.sh" ] && \. "${NVM_DIR}/nvm.sh"  # This loads nvm
fi


which keychain &> /dev/null
if [ $? -ne 0 ]; then
    echo "Keychain not installed, not starting ssh-agent."
else
    ssh_private_keys=$(find ~/.ssh -type f -name 'id_*' | grep -vE '\.pub')
    keychain --quiet --nogui ${ssh_private_keys}
    unset ssh_
    source $HOME/.keychain/$HOSTNAME-sh
fi

which screenfetch &>/dev/null
if [ $? -eq 0 ]; then screenfetch ; fi

source ${HOME}/git/dot-files/zsh/completion.zsh
fpath+=~/.zfunc


autoload -U compinit; compinit
zstyle ':completion:*' menu select

autoload -Uz compinit
