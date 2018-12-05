
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
	# EMACS
	#
	ln -s $(pwd)/emacs		~/
	ln -s ~/emacs/emacs.el		~/.emacs
	(										\
		if [ ! -d ~/emacs/python-mode ]; then					\
			cd ~/emacs;							\
			git clone https://gitlab.com/python-mode-devs/python-mode.git;	\
		fi;									\
		if [ ! -d ~/emacs/go-mode.el ]; then					\
			git clone https://github.com/dominikh/go-mode.el;		\
		fi;									\
	)
	#
	# Other
	#
	ln -s $(pwd)/bin		~/
	ln -s $(pwd)/screenrc		~/.screenrc

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
	rm -f ~/.screenrc


