#!/bin/sh
#
# ~/.profile - portable SH shell config

if [ -x ~/bin/os ]; then
	os=`~/bin/os`
	if [ -f ~/.profile-$os ]; then
		source ~/.profile-$os
	fi
fi

source ~/.bashrc

