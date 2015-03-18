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


##
# Your previous /Users/mjk/.profile file was backed up as /Users/mjk/.profile.macports-saved_2014-12-16_at_11:52:27
##

# MacPorts Installer addition on 2014-12-16_at_11:52:27: adding an appropriate PATH variable for use with MacPorts.
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
# Finished adapting your PATH environment variable for use with MacPorts.

