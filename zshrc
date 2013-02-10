autoload -Uz compinit promptinit vcs_info
compinit
promptinit
autoload zcalc

######################################
# Variables
######################################
export BROWSER="chromium"
export EDITOR="vim"
export PATH="$PATH:$HOME/bin"

### watch ###
watch=( all )
export LOGCHECK=30
export WATCHFMT=$'\e[00;00m\e[01;36m'" -- %n@%m has %(a.logged in.logged out) --"$'\e[00;00m'

### android build variables ###
export USE_CCACHE=1
export CCACHE_DIR=~/.ccache

prompt walters

zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache
zstyle ':vcs_info:*' enable git cvs svn

zstyle ':completion:*:*:pacman:*'	menu yes select
zstyle ':completion:*:pacman:*'	force-list always

zstyle ':completion:*:*:make:*'	menu yes select
zstyle ':completion:*:make:*'	force-list always

zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*'	force-list always

zstyle ':completion:*:*:killall:*' menu yes select
zstyle ':completion:*:killall:*'	force-list always

alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias EWS='ssh jankaus2@remlnx.ews.illinois.edu'
alias cdh='urxvt -cd $(pwd) &'
alias sys='sudo systemctl'
compdef _systemctl sys=systemctl
# todo.sh configs
alias t='todo.sh'
export TODOTXT_DEFAULT_ACTION=pv
compdef _todo.sh t=todo.sh
alias -s pdf=zathura
#alias -g SVN242='https://subversion.ews.illinois.edu/svn/sp13-cs242/jankaus2'
SVN242="https://subversion.ews.illinois.edu/svn/sp13-cs242/jankaus2"

function ranger-cd {
	tempfile='/tmp/.ranger.ranger-cd'
	/usr/bin/ranger --choosedir="$tempfile" "${@:-$(pwd)}"
	test -f "$tempfile" &&
	if [  "$(cat -- "$tempfile")" != "$(echo -n `pwd`)" ]; then
		cd -- "$(cat "$tempfile")"
	fi
	rm -f -- "$tempfile"
}

bindkey -e

# bind Ctrl-O to ranger-cd:
bindkey -s '^O' '^qranger-cd\n'

##### Functions ############
# password generator
function genpass() {
	if [ ! "$1" ]; then
		echo "Usage: $0 20"
		echo "for a random, 20 char password."
		return 1
	fi
	dd if=/dev/urandom count=1 2>/dev/null | tr -cd 'A-Za-z0-9!@#$%^&*()_+' |
		cut -c-$1
}

# Systemd Shortcuts
start() {
	sudo systemctl start $1.service
}

restart() {
	sudo systemctl restart $1.service
}

stop() {
	sudo systemctl stop $1.service
}

enable() {
	sudo systemctl enable $1.service
}

status() {
	sudo systemctl status $1.service
}

disable() {
	sudo systemctl disable $1.service
}

if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
	exec startx
fi
