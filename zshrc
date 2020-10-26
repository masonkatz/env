#!/bin/zsh
#
# ~/.zshrc - portable ZSH shell config

if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
	source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

export ZSH=~/.oh-my-zsh
ZSH_THEME="powerlevel10k/powerlevel10k"
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

CASE_SENSITIVE="true"

plugins=(git git-flow golang docker docker-compose jsontools kubectl)


if [ -x ~/bin/os ]; then
	os=`~/bin/os`
	[[ ! -f ~/.zshrc-$os ]] || source ~/.zshrc-$os
fi



if [ -d ~/go/bin ]; then
	PATH=$PATH:~/go/bin
fi

DEFAULT_USER=`whoami`

source $ZSH/oh-my-zsh.sh

# Do this after zsh setup to fix the ls aliases

case $TERM in
eterm-color)
	alias ls='ls -F --color'
	DISABLE_AUTO_TITLE="true"
	;;
xterm*|screen*color)
	alias ls='ls -F --color'
        ;;
esac

alias l='ls'
alias la='ls -a'
alias ll='ls -l'
unalias lsa

alias m='make'


