HOSTNAME="$(hostname)"

EBI_DOMAIN="ebi.ac.uk"

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

is_work() {
    test "${HOSTNAME#*$EBI_DOMAIN}" != "$HOSTNAME";
    return $?;
}

if [ is_work ]; then
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
alias pbcopy="xclip -selection clipboard";
alias pbpaste="xclip -selection clipboard -o";

source "$ZSH/oh-my-zsh.sh"

if [ ! -z "$DISPLAY" ]; then
    setxkbmap -layout us -option ctrl:nocaps
fi
