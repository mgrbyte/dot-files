# Path to your oh-my-zsh installation.

plugins=(git python virtualenv-prompt virtualenvwrapper pip fabric debian themes)

clean_old_kernels () {
    sudo apt-get purge $(dpkg --list | \
			 grep -E 'linux-image-[0-9]' | \
			 awk '{ print $3,$2 }' | \
			 sort -nr | \
                         tail -n +2 | \
			 grep -v $(uname -r) | \
			 awk '{ print $2 }')
    return $?;
}

# XXX: according to the manuals, this file should not assume a tty
# or run commands that produce output, which the folowing technically does..sorta.
# Make sure /usr/local is before /usr/bin so custom stuff gets preference
# export PATH="/usr/local/bin:$(echo $PATH | sed 's|/usr/local/bin:||g')"

alias cljsbuild="lein trampoline cljsbuild $@"
alias ls="ls --group-directories-first -p"
alias ll="ls -g"
alias la="ls -a"
alias lt="ls -lt"
alias grep="grep --color=auto"
alias pbcopy="xclip -selection clipboard";
alias pbpaste="xclip -selection clipboard -o";

source "$ZSH/oh-my-zsh.sh"
source "$VENV_WRAPPER"

setxkbmap -layout us -option ctrl:nocaps
