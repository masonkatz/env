
OS=$(shell ./bin/os)

home=$(shell echo $$HOME/ | sed 's/\//\\\//g')
pwd=$(shell echo $(PWD) | sed 's/$(home)//g')

default:
	@echo Running on $(OS)

install clean::
	$(MAKE) -f Makefile-$(OS) $@

install::
	#
	# Git
	#
	ln -s $(pwd)/gitconfig		~/.gitconfig
	#
	# BASH config
	#
	ln -s $(pwd)/bashrc		~/.bashrc
	ln -s $(pwd)/bashrc-macosx	~/.bashrc-macosx
	ln -s $(pwd)/bashrc-linux	~/.bashrc-linux
	ln -s $(pwd)/profile		~/.profile
	ln -s $(pwd)/profile-macosx	~/.profile-macosx
	ln -s $(pwd)/dircolors		~/.dircolors
	#
	# Other
	#
	ln -s $(pwd)/bin		~/
	ln -s $(pwd)/emacs		~/
	ln -s $(pwd)/emacs/emacs.el	~/.emacs
	#
	# SSH
	#
	if [ ! -d ~/.ssh ]; then		\
		mkdir ~/.ssh;			\
		chmod 700 ~/.ssh;		\
	fi
	install -m644 sshconfig ~/.ssh/config

clean::
	rm -f ~/.gitconfig
	rm -f ~/.bashrc
	rm -f ~/.bashrc-macosx
	rm -f ~/.bashrc-linux
	rm -f ~/.profile
	rm -f ~/.profile-macosx
	rm -f ~/.profile-linux
	rm -f ~/.bash_profile
	rm -f ~/.dircolors
	rm -f ~/bin
	rm -f ~/emacs
	rm -f ~/.emacs


