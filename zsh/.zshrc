HOSTNAME="$(hostname)"

# Hint: Disable advanced theme for emacs tramp compat by settng ZSH_THEME="".
if [[ "${TERM}" = "PS1" ]]; then
   ZSH_THEME=""
else
    ZSH_THEME="socrates"
fi
export ZSH_THEME

plugins=(git python virtualenvwrapper pip fabric debian themes)


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
alias python="python3"
alias pip="python3 -m pip"

alias psj="ps awx | grep 'jav[a]'"
alias rgrep-clj='grep --include="*.clj" -r'

source "$ZSH/oh-my-zsh.sh"

DISPLAY=""
WSL=$(which wsl.exe)
if [ $? -eq 0  ] && [ -z "$DISPLAY" ]; then
    DISPLAY="$(grep -m 1 nameserver /etc/resolv.conf | awk '{print $2}')"
fi

if [ ! -z "$DISPLAY" ]; then
    export DISPLAY="$DISPLAY:0"
    setxkbmap -layout us -option ctrl:nocaps
else
    echo "X is not running!"
fi

which keychain > /dev/null 2>&1
if [ ! $? -eq 0 ]; then
    echo "Keychain not installed, not starting ssh-agent."
else
    keychain --quiet --nogui /home/matt/.ssh/id_rsa_mtr21pqh_bangor_ac_uk
    source $HOME/.keychain/$HOST-sh
fi

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
