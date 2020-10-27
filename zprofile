#!/bin/zsh
#
# ~/.zprofile - portable ZSH shell config

if [ -x ~/bin/os ]; then
	os=`~/bin/os`
	if [ -f ~/.zprofile-$os ]; then
		source ~/.zprofile-$os
	fi
fi

[[ -d ~/go/bin ]] || mkdir -p ~/go/bin

export GOPATH=~/go

