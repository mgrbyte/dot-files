# conditionals

HOMEBREW="/opt/homebrew"
if [ -d "${HOMEBREW}" ]; then
    PATH="${HOMEBREW}/bin:${PATH}"
fi

CASK="${HOME}/.cask"
if [ -e "${CASK}" ]; then
    PATH="${PATH}:${CASK}/bin"
fi

DATOMIC="${HOME}/datamoic"
if [ -e "${DATOMIC}" ]; then
    PATH="${PATH}:${DATOMIC}/bin"
fi

NIXOS="${HOME}/.nix-profile"
if [ -e "${NIXOS}" ]; then
    PATH="${PATH}:${NIXOS}/bin"
fi

LOCAL_BIN="${HOME}/.local/bin"
if [ -e "${LOCAL_BIN}" ]; then
    PATH="${LOCAL_BIN}:${PATH}"
fi

ANDROID_STUDIO="${HOME}/android-studio"
if [ -d "${ANDROID_STUDIO}" ]; then
    PATH="${PATH}:${ANDROID_STUDIO}/bin"
fi

ARDUINO_HOME="${HOME}/arduino-1.8.5"
if [ -d "${ARDUINO_HOME}" ]; then
    PATH="${PATH}:${ARDUINO_HOME}"
fi

TERRAFORM_BIN="${HOME}/terraform"
if [ -d "${TERRAFORM_BIN}" ]; then
    PATH="${TERRAFORM_BIN}:${PATH}"
fi

# AWS Java CLI
EC2_HOME="${HOME}/ec2-api-tools"
if [ -e "${EC2_HOME}" ]; then
    PATH="${PATH}:${EC2_HOME}/bin"
fi

AWS_ENV_FILE="${HOME}/.aws/env"
if [ -e "${AWS_ENV_FILE}" ]; then
    source "${AWS_ENV_FILE}"
fi

LOCAL_EMACS="$HOME/.local/emacs"
if [ -d ${LOCAL_EMACS} ]; then
    PATH="${LOCAL_EMACS}/bin:${PATH}"
fi
MAMBA_HOME="$HOME/mambaforge"
if [ -d $MAMBA_HOME ]; then
    export MAMBA_HOME
    PATH="${MAMBA_HOME}/bin:$PATH"
fi


export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"

which code &> /dev/null
if [ $? -eq 0 ]; then
    export EDITOR="code"
    export VISUAL="code"
else
    export EDITOR="emacs"
    export VISUAL="emacs"
fi


export DOTFILES="${HOME}/github/mgrbyte/dot-files"

# Turn off making distinction between output ending with \newline or not
export PROMPT_EOL_MARK=""

# general
export EMAIL="$(git config user.email)"
export GIT_TEMPLATES_DIR="${HOME}/.git-templates"
export GREP_COLOR="33;51;1"
export LANGUAGE="en_GB:en"
export LC_COLLATE="en_GB.utf8"
export LC_CTYPE="en_GB.utf8"
export LC_MESSAGES="en_GB.utf8"
export NAME="Matt Russell"

# ohmyzsh
export ZSH="${HOME}/.oh-my-zsh"
export ZSH_CUSTOM="${ZSH}/custom"
export ZSH_THEME="jreese"
export CASE_SENSITIVE="true"
export ENABLE_CORRECTION="false"
export APPEND_HISTORY="1"
export INC_APPEND_HISTORY="1"
export HISTFILE="${HOME}/.zsh_history"
export SAVEHIST=1000000
export HIST_STAMPS="dd/mm/yyyy"
export COMPLETION_WAITING_DOTS="true"

# XDG
export XDG_DESKTOP_DIR="${HOME}/Desktop"
export XDG_DOWNLOAD_DIR="${HOME}/Downloads"
export XDG_TEMPLATES_DIR="${HOME}/Templates"
export XDG_PUBLICSHARE_DIR="${HOME}/Public"
export XDG_DOCUMENTS_DIR="${HOME}/Documents"
export XDG_MUSIC_DIR="${HOME}/Music"
export XDG_PICTURES_DIR="${HOME}/Pictures"
export XDG_VIDEOS_DIR="${HOME}/Videos"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_CONFIG_HOME="${DOTFILES}"
export ZDOTDIR="${XDG_CONFIG_HOME}/dot-zsh"

# Load work env if present
if [ -e "${HOME}/.work.env" ]; then
    source "${HOME}/.work.env"
fi

# Always export PATH last after all manipulations.
export PATH
