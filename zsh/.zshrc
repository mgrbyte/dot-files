HOSTNAME="$(hostname)"

EBI_DOMAIN="ebi.ac.uk"

is_ebi_host() {
    test "${HOSTNAME#*$EBI_DOMAIN}" != "$HOSTNAME";
    return $?;
}

if [ is_ebi_host ]; then
    # Hint: Disable advanced theme for emacs tramp compat by settng ZSH_THEME="".
    export ZSH_THEME="socrates"
    plugins=(git python themes)
else
    plugins=(git python virtualenv-prompt virtualenvwrapper pip fabric debian themes)
fi

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

alias psj="ps awx | grep 'jav[a]'"
alias rgrep-clj='grep --include="*.clj" -r'

source "$ZSH/oh-my-zsh.sh"

if [ ! -z "$DISPLAY" ]; then
    setxkbmap -layout us -option ctrl:nocaps
fi

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"
