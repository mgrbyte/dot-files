# Path to your oh-my-zsh installation.
export ZSH=/home/mattr/.oh-my-zsh

ZSH_CUSTOM="$HOME/oh-my-zsh-custom"
ZSH_THEME="socrates"
CASE_SENSITIVE="true"
ENABLE_CORRECTION="true"
HIST_STAMPS="dd/mm/yyyy"
COMPLETION_WAITING_DOTS="true"

plugins=(git python virtualenv virtualenvwrapper pip fabric debian themes)

export PATH="/home/mattr/.local/bin:/usr/local/bin:/home/mattr/.local/bin:/usr/local/sbin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/home/mattr/.cask/bin:/home/mattr/.local/bin:/home/mattr/.cask/bin:/home/mattr/.local/bin"
export MANPATH="/usr/local/man:$MANPATH"
export SSH_KEY_PATH="~/.ssh/id_rsa"

source ~/.profile
source $ZSH/oh-my-zsh.sh

