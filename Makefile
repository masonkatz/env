
OS=$(shell ./bin/os)

home=$(shell echo $$HOME/ | sed 's/\//\\\//g')
pwd=$(shell echo $(PWD) | sed 's/$(home)//g')

default:
	@echo Running on $(OS)

install: zsh-install.sh
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
	# ZSH config
	#
	sh ./zsh-install.sh --unattended; rm -f ~/.zshrc
	ln -s $(pwd)/zshrc		~/.zshrc
	ln -s $(pwd)/zshrc-macosx	~/.zshrc-macosx
	ln -s $(pwd)/zshrc-linux	~/.zshrc-linux
	ln -s $(pwd)/zprofile		~/.zprofile
	ln -s $(pwd)/zprofile-macosx	~/.zprofile-macosx
	ln -s $(pwd)/p10k.zsh		~/.p10k.zsh
	if [ ! -d ~/.oh-my-zsh/custom/themes/powerlevel10k ]; then			\
		git clone https://github.com/romkatv/powerlevel10k.git			\
			~/.oh-my-zsh/custom/themes/powerlevel10k;			\
	fi
	#
	# EMACS
	#
	ln -s $(pwd)/emacs		~/
	ln -s ~/emacs/emacs.el		~/.emacs
	(										\
		cd ~/emacs;								\
		if [ ! -d python-mode ]; then						\
			git clone https://gitlab.com/python-mode-devs/python-mode.git;	\
		fi;									\
		if [ ! -d go-mode.el ]; then						\
			git clone https://github.com/dominikh/go-mode.el;		\
		fi;									\
		if [ ! -d dockerfile-mode ]; then					\
			git clone git@github.com:masonkatz/dockerfile-mode.git;		\
		fi;									\
		if [ ! -d yaml-mode ]; then						\
			git clone https://github.com/yoshiki/yaml-mode.git;		\
		fi;									\
		if [ ! -d tramp-term.el ]; then						\
			git clone https://github.com/randymorris/tramp-term.el.git;	\
		fi;									\
		if [ ! -d web-mode ]; then						\
			git clone https://github.com/fxbois/web-mode;			\
		fi;									\
		if [ ! -d prettier-emacs ]; then					\
			git clone https://github.com/prettier/prettier-emacs.git;	\
		fi									\
	)
	#
	# Other
	#
	ln -s $(pwd)/bin		~/
	ln -s $(pwd)/screenrc		~/.screenrc


zsh-install.sh:
	[ -d $@ ] || mkdir -p $(dir $@)
	curl -o $@ -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh

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
	rm -f ~/.zshrc
	rm -f ~/.zshrc-macosx
	rm -f ~/.zshrc-linux
	rm -f ~/.zprofile
	rm -f ~/.zprofile-macosx
	rm -f ~/.zprofile-linux
	rm -f ~/.p10k.zsh
	rm -f ~/bin
	rm -f ~/emacs
	rm -f ~/.emacs
	rm -f ~/.screenrc
	rm -rf ~/.oh-my-zsh zsh-install.sh


