#!/bin/sh
#
# ~/.profile - portable SH shell config

PS_INFO='\u@\h '

if [ -x ~/bin/os ]; then
	os=`~/bin/os`
	if [ -f ~/.profile-$os ]; then
		source ~/.profile-$os
	fi
fi

source ~/.bashrc


test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"

