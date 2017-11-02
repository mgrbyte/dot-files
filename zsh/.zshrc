HOSTNAME="$(hostname)"

EBI_DOMAIN="ebi.ac.uk"

clean_old_kernels () {
    sudo apt-get purge $(dpkg --list | \
			 grep -E 'linux-image-[0-9]' | \
			 awk '{ print $3,$2 }' | \
			 sort -nr | \
                         tail -n +2 | \
			 grep -v $(uname -r) | \
			 awk '{ print $2 }');
    if [ $? -eq 0 ]; then
	sudo apt-get autoremove
    fi
    if [ $? -eq 0 ]; then
	sudo apt-get clean
    fi
    return $?;
}

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
