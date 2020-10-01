#!/bin/zsh
#
# ~/.zshrc - portable ZSH shell config

if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

export ZSH=~/.oh-my-zsh

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
#ZSH_THEME="agnoster"

ZSH_THEME="powerlevel10k/powerlevel10k"
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

CASE_SENSITIVE="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

plugins=(kubectl)


if [ -x ~/bin/os ]; then
	os=`~/bin/os`
	if [ -f ~/.zshrc-$os ]; then
		source ~/.zshrc-$os
	fi
fi



if [ -d ~/go/bin ]; then
	PATH=$PATH:~/go/bin
fi

DEFAULT_USER=`whoami`

source $ZSH/oh-my-zsh.sh

# Do this after zsh setup to fix the ls aliases

case $TERM in
xterm*|screen*color)
	alias ls='ls -F --color'
	alias la='ls -Fa --color'
        ;;
eterm-color)
	DISABLE_AUTO_TITLE="true"
	alias ls='ls -F --color'
	alias la='ls -Fa --color'
	;;
*)
	alias ls='ls -F'
	alias la='ls -Fa'
	;;
esac

