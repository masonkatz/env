#!/bin/zsh
#
# ~/.zshenv - portable ZSH shell config (read first)

[[ -d ~/go/bin ]] || mkdir -p ~/go/bin
export GOPATH=~/go
PATH=$PATH:$GOPATH/bin

if [ -d /opt/softiron/build/rules ]; then
	export SI_BUILD=/opt/softiron/build/rules
fi

if [ -x ~/bin/os ]; then
	os=`~/bin/os`
	if [ -f ~/.zshenv-$os ]; then
		source ~/.zshenv-$os
	fi
fi





