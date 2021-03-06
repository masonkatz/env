# * Makefile

OS := $(shell ./bin/os)

SITE := personal

home := $(shell echo $$HOME/ | sed 's/\//\\\//g')
pwd  := $(shell echo $(PWD) | sed 's/$(home)//g')

.PHONY: help
help:
	@echo "Usage: make [SITE=personal|softiron|kassette] [target]"
	@grep -oh -E '^[a-zA-Z_-]+:.*?##.*$$' Makefile | sort | \
	awk 'BEGIN {FS = ":.*?## "}; {printf "%-16s  %s\n", $$1, $$2}'

default:
	@echo Running on $(OS)

# * Git

install-git: ~/.gitconfig ## 

clean-git: ## 
	rm -f ~/.gitconfig

nuke-git: clean-git

~/.gitconfig: gitconfig
	@echo Setting Git for SITE=$(SITE)
	cp gitconfig $@
	@echo "[user]" >> $@
ifeq ($(strip $(SITE)),personal)
	@echo "        name = Mason J. Katz" >> $@
	@echo "        email = Mason.Katz@gmail.com" >> $@
endif
ifeq ($(strip $(SITE)),softiron)
	@echo "        name = Mason Katz" >> $@
	@echo "        email = Mason.Katz@SoftIron.com" >> $@
endif
ifeq ($(strip $(SITE)),kassette)
	@echo "        name = Mason Katz" >> $@
	@echo "        email = Mason.Katz@Kassette.com" >> $@
endif

# * ZSH

install-zsh: ~/.oh-my-zsh/custom/themes/powerlevel10k ##
	echo SITE=$(SITE)		> ~/.zshrc-site
	ln -s $(pwd)/zshrc		~/.zshrc
	ln -s $(pwd)/zshrc-softiron	~/.zshrc-softiron
	ln -s $(pwd)/zshrc-macosx	~/.zshrc-macosx
	ln -s $(pwd)/zshrc-linux	~/.zshrc-linux
	ln -s $(pwd)/zprofile		~/.zprofile
	ln -s $(pwd)/zprofile-macosx	~/.zprofile-macosx
	ln -s $(pwd)/zshenv		~/.zshenv
	ln -s $(pwd)/zshenv-macosx	~/.zshenv-macosx
	ln -s $(pwd)/zshenv-linux	~/.zshenv-linux
	ln -s $(pwd)/p10k.zsh		~/.p10k.zsh

clean-zsh: ## 
	rm -f ~/.zshrc
	rm -f ~/.zshrc-size
	rm -f ~/.zshrc-softiron
	rm -f ~/.zshrc-macosx
	rm -f ~/.zshrc-linux
	rm -f ~/.zprofile
	rm -f ~/.zprofile-macosx
	rm -f ~/.zprofile-linux
	rm -f ~/.zshenv
	rm -f ~/.zshenv-macosx
	rm -f ~/.zshenv-linux
	rm -f ~/.p10k.zsh
	rm -f ~/.oh-my-zsh/custom/themes/powerlevel10k

nuke-zsh: clean-zsh
	rm -rf ~/.oh-my-zsh zsh-install.sh
	rm -rf powerlevel10k

zsh-install.sh:
	curl -o $@ -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh

~/.oh-my-zsh/custom/themes/powerlevel10k: ~/.oh-my-zsh
	[ -d powerlevel10k ] || git clone https://github.com/romkatv/powerlevel10k
	cd powerlevel10k && git pull
	ln -s ../../../$(pwd)/powerlevel10k $@

~/.oh-my-zsh: zsh-install.sh
	[ -d ~/.oh-my-zsh ] || (sh ./zsh-install.sh --unattended && rm -f ~/.zshrc)

# * BASH

install-bash: ## 
	ln -s $(pwd)/bashrc		~/.bashrc
	ln -s $(pwd)/bashrc-macosx	~/.bashrc-macosx
	ln -s $(pwd)/bashrc-linux	~/.bashrc-linux
	ln -s $(pwd)/profile		~/.profile
	ln -s $(pwd)/profile-macosx	~/.profile-macosx

clean-bash: ## 
	rm -f ~/.bashrc
	rm -f ~/.bashrc-macosx
	rm -f ~/.bashrc-linux
	rm -f ~/.profile
	rm -f ~/.profile-macosx
	rm -f ~/.profile-linux
	rm -f ~/.bash_profile

nuke-bash: clean-bash

# * EMACS

install-emacs: ##
	ln -s $(pwd)/emacs	~/
	ln -s emacs/emacs.el	~/.emacs

clean-emacs: ## 
	rm -f ~/emacs ~/.emacs

nuke-emacs: clean-emacs
	rm -rf ~/.emacs.d


# * Go

export GOPATH=$(HOME)/go

install-go:
	if which go; then \
		go get -u golang.org/x/lint/golint; \
		go get -u golang.org/x/tools/cmd/goimports; \
		go get -u github.com/nsf/gocode; \
	fi


# * main


install: install-git install-zsh install-bash install-emacs install-go ## install entire environment
ifeq ($(OS),macosx)
	[ -d ~/.ssh ] || mkdir -m700 .ssh
	ln -s ../$(pwd)/ssh-config	~/.ssh/config
endif
	ln -s $(pwd)/dircolors	~/.dircolors
	ln -s $(pwd)/bin	~/
	ln -s $(pwd)/screenrc	~/.screenrc

clean: clean-git clean-bash clean-zsh clean-emacs ## removes local config (do this first)
ifeq ($(OS),macosx)
	rm -f ~/.ssh/config
endif
	rm -f ~/.dircolors
	rm -f ~/bin
	rm -f ~/.screenrc

nuke: clean nuke-git nuke-zsh nuke-bash nuke-emacs ## removes everything

kassette: ## reset for kassette config
	$(MAKE) clean install SITE=$@

softiron: ## reset for softiron config
	$(MAKE) clean install SITE=$@

personal: ## reset for personal config
	$(MAKE) clean install

