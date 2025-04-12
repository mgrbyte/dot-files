ZSH_THEME="sonicradish" # set by `omz`

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
    keychain --agents "ssh,gpg" --quick --quiet --nogui ${ssh_private_keys}
    unset ssh_private_keys
    source ${HOME}/.keychain/$(hostname)-sh
fi

which screenfetch &>/dev/null
if [ $? -eq 0 ]; then screenfetch ; fi

source ${DOTFILES}/zsh/completion.zsh
fpath+=~/.zfunc

autoload -U compinit; compinit
zstyle ':completion:*' menu select
zstyle ':completion:*:*:ssh:*:*' known-hosts-files /etc/ssh/ssh_known_hosts ~/.ssh/known_hosts
autoload -Uz compinit
