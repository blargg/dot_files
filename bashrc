
# load shell settings
if [ -d "${HOME}/.commonsh" ] ; then
	for file in $(ls $HOME/.commonsh/) ; do
		. "${HOME}/.commonsh/${file}"
	done
fi
