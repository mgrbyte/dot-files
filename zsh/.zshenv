
# conditionals
CASK="${HOME}/.cask"
if [ -e "${CASK}" ]; then
    PATH="${PATH}:${CASK}/bin"
fi

DOT_LOCAL="${HOME}/.local"
if [ -e "${DOT_LOCAL}" ]; then
    PATH="${PATH}:${DOT_LOCAL}/bin"
fi

DATOMIC="${HOME}/datamoic"
if [ -e "${DATOMIC}" ]; then
    PATH="${PATH}:${DATOMIC}/bin"
fi

NIXOS="${HOME}/.nix-profile"
if [ -e "${NIXOS}" ]; then
    PATH="${PATH}:${NIXOS}/bin"
fi

if [ -d "${HOME}/bin" ]; then
    PATH="${PATH}:${HOME}/bin"
fi

LOCAL="${HOME}/.local"
if [ -d "${LOCAL}/bin" ]; then
    PATH="${LOCAL}/bin:${PATH}"
fi

VENV_WRAPPER="${HOME}/.local/bin/virtualenvwrapper.sh"
if [ -f "${VENV_WRAPPER}" ]; then
    VIRTUALENVWRAPPER_PYTHON="$(which python3)"
    source "${VENV_WRAPPER}"
fi

ANDROID_STUDIO="${HOME}/android-studio"
if [ -d "${ANDROID_STUDIO}" ]; then
    PATH="${PATH}:${ANDROID_STUDIO}/bin"
fi

linuxbrew_home="/home/linuxbrew/.linuxbrew"
if [ -d "${linuxbrew_home}" ]; then
    HOMEBREW_PREFIX="${linuxbrew_home}"
    unset linuxbrew_home
    PATH="${HOMEBREW_PREFIX}/bin:${HOMEBREW_PREFIX}/sbin:$PATH"
    HOMEBREW_PREFIX="$(brew --prefix)"
    export HOMEBREW_CELLAR="${HOMEBREW_PREFIX}/Cellar"
    export HOMEBREW_REPOSITORY="${HOMEBREW_PREFIX}/Homebrew"
    export MANPATH="${HOMEBREW_PREFIX}/share/man:$MANPATH"
    export INFOPATH="${HOMEBREW_PREFIX}/share/info:$INFOPATH"
else
    echo "Not setting up homebrew, has not been installed."

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

# Turn off making distinction between output ending with \newline or not
export PROMPT_EOL_MARK=""

# general
export EDITOR="emacsclient -t"
export EMAIL="matt@mgrbyte.co.uk"
export GIT_TEMPLATES_DIR="${HOME}/.git-templates"
export GREP_COLOR="33;51;1"
export LANGUAGE="en_GB:en"
export LC_COLLATE="en_GB.utf8"
export LC_CTYPE="en_GB.utf8"
export LC_MESSAGES="en_GB.utf8"
export LEIN_FAST_TRAMPOLINE="y"
export NAME="Matt Russell"
export SMTPSERVER="smtp.hosts.co.uk"
export SMTPUSER="mgrbyte.co.uk"
export VISUAL="emacsclient -c -a ''"
export WORKON_HOME="${HOME}/.virtualenvs"
export PROJECT_HOME="${HOME}/git"

# ohmyzsh
export ZSH="${HOME}/.oh-my-zsh"
export ZSH_CUSTOM="${HOME}/oh-my-zsh-custom"
export ZSH_THEME="socrates"
export CASE_SENSITIVE="true"
export ENABLE_CORRECTION="false"
export APPEND_HISTORY="1"
export INC_APPEND_HISTORY="1"
export HISTFILE="${HOME}/.history"
export SAVEHIST=1000000
export HIST_STAMPS="dd/mm/yyyy"
export COMPLETION_WAITING_DOTS="true"

AWS_ENV_FILE="${HOME}/.aws/env"
if [ -e "${AWS_ENV_FILE}" ]; then
    source "${AWS_ENV_FILE}"
elif [ -e "$(dirname ${AWS_ENV_FILE})" ]; then
    echo "WARNING: AWS environment file ${AWS_ENV_FILE} is missing"
fi

# Always export PATH last after all manipulations.
export PATH
