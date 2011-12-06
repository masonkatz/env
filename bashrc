#!/bin/bash
#
# ~/.bashrc - portable BASH shell config

ansi_reset=0
ansi_bright=1
ansi_dim=2
ansi_underscore=4
ansi_blink=5
ansi_reverse=7
ansi_hidden=8

ansi_fblack=30
ansi_fred=31
ansi_fgreen=32
ansi_fyellow=33
ansi_fblue=34
ansi_fmagenta=35
ansi_fcyan=36
ansi_fwhite=37

ansi_bblack=40
ansi_bred=41
ansi_bgreen=42
ansi_byellow=43
ansi_bblue=44
ansi_bmagenta=45
ansi_bcyan=46
ansi_bwhite=47


function git_prompt() {
	local git_status="`git status -unormal 2>&1`"
	if ! [[ "$git_status" =~ Not\ a\ git\ repo ]]; then
		if [[ "$git_status" =~ nothing\ to\ commit ]]; then
			local color=$ansi_fgreen
		elif [[ "$git_status" =~ nothing\ added\ to\ commit\ but\ untracked\ files\ present ]]; then
			local color=$ansi_fyellow
		else
			local color=$ansi_fred
		fi
		if [[ "$git_status" =~ On\ branch\ ([^[:space:]]+) ]]; then
			branch=${BASH_REMATCH[1]}
		else
            	# Detached HEAD.  (branch=HEAD is a faster alternative.)
			branch="(`git describe --all --contains --abbrev=4 HEAD 2> /dev/null || echo HEAD`)"
		fi
		echo -n "\[\e[$ansi_reset;$ansi_underscore;${color}m\]$branch\[\e[${ansi_reset}m\] "
	fi
}

function prompt_command() {
	local pwd_length=32
	local DIR=`pwd`

	echo $DIR | grep "^$HOME" >> /dev/null
	if [ $? -eq 0 ]; then
		CURRDIR=`echo $DIR | awk -F$HOME '{print $2}'`
		newPWD="~$CURRDIR"

		if [ $(echo -n $newPWD | wc -c | tr -d " ") -gt $pwd_length ]; then
			newPWD="~/..$(echo -n $PWD | sed -e "s/.*\(.\{$pwd_length\}\)/\1/")"
		fi
	elif [ "$DIR" = "$HOME" ]; then
		newPWD="~"
	elif [ $(echo -n $PWD | wc -c | tr -d " ") -gt $pwd_length ]; then
		newPWD="..$(echo -n $PWD | sed -e "s/.*\(.\{$pwd_length\}\)/\1/")"
	else
		newPWD="$(echo -n $PWD)"
	fi

	if [ $EMACS ]; then
		PS1="\W \$ "
	else
		PS1="\[\e]0;\u@\h\a\]\[\e[${ansi_fwhite}m\]\h `git_prompt`\[\e[${ansi_fcyan}m\]$newPWD \[\e[${ansi_fmagenta}m\]\\$\[\e[${ansi_reset}m\] "
	fi
}

PROMPT_COMMAND=prompt_command


CVS_RSH=ssh
export CVS_RSH


case $TERM in
xterm*)
	alias ls='$ls -F $colorls'
	alias la='$ls -Fa $colorls'
        ;;
*)
	alias ls='$ls -F'
	alias la='$ls -Fa'
	;;
esac


if [ -x ~/bin/os ]; then
	os=`~/bin/os`
	if [ -f ~/.bashrc-$os ]; then
		source ~/.bashrc-$os
	fi
fi

export EDITOR="emacs -nw"
