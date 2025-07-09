ZSH_THEME="sonicradish" # set by `omz`

plugins=(git python themes)

alias cljsbuild="lein trampoline cljsbuild $@"
alias ls="ls --group-directories-first -p"
alias ll="ls -g"
alias la="ls -a"
alias lt="ls -lt"
alias grep="grep --color=auto"
alias egrep="egrep --color=auto"
alias rgrep="grep -r --color=auto"
# alias pbcopy="xclip -selection clipboard";
# alias pbpaste="xclip -selection clipboard -o";

alias psj="ps awx | grep 'jav[a]'"
alias rgrep='grep -H -r -n'
alias rgrep-ts='rgrep --include="*.ts" --include="*.tsx"'
alias rgrep-clj='rgrep --include="*.clj" -r'
alias rgrep-py='rgrep --include="*.py"'

source "${ZSH}/oh-my-zsh.sh"

which setxbmap &> /dev/null
if [ $? -eq 0 ]; then
    setxkbmap -layout us -option ctrl:nocaps
fi

export NVM_DIR="$HOME/.nvm"
if [ -d ${NVM_DIR} ]; then
    [ -s "${NVM_DIR}/nvm.sh" ] && \. "${NVM_DIR}/nvm.sh"  # This loads nvm
fi


which keychain &> /dev/null
if [ $? -eq 0 ]; then
    ssh_private_keys=$(grep -slR "PRIVATE" ~/.ssh/)
    keychain --quick --quiet --nogui ${ssh_private_keys}
    unset ssh_private_keys
    source ${HOME}/.keychain/$(hostname)-sh
fi

which screenfetch &>/dev/null
if [ $? -eq 0 ]; then screenfetch ; fi

source ${DOTFILES}/zsh/completion.zsh
fpath+=~/.zfunc

# Load any work specific env vars.
[ -f "~/.zshenv.work" ] && source ~/.zshenv.work

autoload -U compinit; compinit
zstyle ':completion:*' menu select
zstyle ':completion:*:*:ssh:*:*' known-hosts-files /etc/ssh/ssh_known_hosts ~/.ssh/known_hosts
autoload -Uz compinit
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
