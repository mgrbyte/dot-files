source ~/.profile

export PS1="[%n in %d on %M]$ "
export HISTSIZE=5000
export HISTFILE="$HOME/.zhistory"

setopt hist_ignore_all_dups
setopt hist_ignore_space
setopt correctall
setopt autocd

autoload -U compinit
compinit

autoload -U promptinit
promptinit
prompt adam2


