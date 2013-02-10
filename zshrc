autoload -Uz compinit promptinit vcs_info
compinit
promptinit
prompt walters
autoload zcalc
bindkey -e

### watch ###
watch=( all )
export LOGCHECK=30
export WATCHFMT=$'\e[00;00m\e[01;36m'" -- %n@%m has %(a.logged in.logged out) --"$'\e[00;00m'

#alias -s pdf=zathura

# bind Ctrl-O to ranger-cd:
bindkey -s '^O' '^qranger-cd\n'

# load shell settings
if [ -d "${HOME}/.commonsh" ] ; then
	for file in $(ls $HOME/.commonsh/) ; do
		. "${HOME}/.commonsh/${file}"
	done
fi

# load zsh specific files
if [ -d "${HOME}/.zsh" ] ; then
	for file in $(ls $HOME/.zsh/) ; do
		. "${HOME}/.zsh/${file}"
	done
fi

if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
	exec startx
fi
