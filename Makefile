OS := $(shell ./bin/os)

PERSONA := personal

home := $(shell echo $$HOME/ | sed 's/\//\\\//g')
pwd  := $(shell echo $(PWD) | sed 's/$(home)//g')

.PHONY: help
help:
	@echo "Usage: make [PERSONA=personal|softiron|kassette] [target]"
	@grep -oh -E '^[a-zA-Z_-]+:.*?## .*$$' Makefile | sort | \
	awk 'BEGIN {FS = ":.*?## "}; {printf "%-16s  %s\n", $$1, $$2}'

default:
	@echo Running on $(OS)

##
## Git
##

install-git: ~/.gitconfig ##

clean-git: ## 
	rm -f ~/.gitconfig

nuke-git: clean-git

~/.gitconfig: gitconfig
	@echo Setting Git for PERSONA=$(PERSONA)
	cp gitconfig $@
	@echo "[user]" >> $@
ifeq ($(strip $(PERSONA)),personal)
	@echo "\tname = Mason J. Katz" >> $@
	@echo "\temail = Mason.Katz@gmail.com" >> $@
endif
ifeq ($(strip $(PERSONA)),softiron)
	@echo "\tname = Mason Katz" >> $@
	@echo "\temail = Mason.Katz@SoftIron.com" >> $@
endif
ifeq ($(strip $(PERSONA)),kassette)
	@echo "\tname = Mason Katz" >> $@
	@echo "\temail = Mason.Katz@Kassette.com" >> $@
endif

##
## ZSH
##

install-zsh: powerlevel10k ## 
	ln -s $(pwd)/zshrc		~/.zshrc
	ln -s $(pwd)/zshrc-macosx	~/.zshrc-macosx
	ln -s $(pwd)/zshrc-linux	~/.zshrc-linux
	ln -s $(pwd)/zprofile		~/.zprofile
	ln -s $(pwd)/zprofile-macosx	~/.zprofile-macosx
	ln -s $(pwd)/p10k.zsh		~/.p10k.zsh

clean-zsh: ## 
	rm -f ~/.zshrc
	rm -f ~/.zshrc-macosx
	rm -f ~/.zshrc-linux
	rm -f ~/.zprofile
	rm -f ~/.zprofile-macosx
	rm -f ~/.zprofile-linux
	rm -f ~/.p10k.zsh

nuke-zsh: clean-zsh
	rm -rf ~/.oh-my-zsh zsh-install.sh
	rm -rf powerlevel10k

zsh-install.sh:
	curl -o $@ -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh

powerlevel10k: ~/.oh-my-zsh
	git clone https://github.com/romkatv/$@
	ln -s ../../../$(pwd)/powerlevel10k ~/.oh-my-zsh/custom/themes/powerlevel10k

~/.oh-my-zsh: zsh-install.sh
	[ -d ~/.oh-my-zsh ] || (sh ./zsh-install.sh --unattended && rm -f ~/.zshrc)


##
## BASH
##

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


##
## EMACS
##

install-emacs: emacs/python-mode emacs/go-mode.el emacs/dockerfile-mode emacs/yaml-mode emacs/tramp-term.el emacs/web-mode emacs/prettier-emacs ## 
	ln -s $(pwd)/emacs		~/
	ln -s ~/emacs/emacs.el		~/.emacs

clean-emacs: ## 
	rm -f ~/emacs
	rm -f ~/.emacs

nuke-emacs: clean-emacs
	rm -rf emacs/python-mode
	rm -rf emacs/go-mode.el
	rm -rf emacs/dockerfile-mode
	rm -rf emacs/yaml-mode
	rm -rf emacs/tramp-term.el
	rm -rf emacs/web-mode
	rm -rf emacs/prettier-emacs

emacs/python-mode:
	git clone https://gitlab.com/python-mode-devs/$(notdir $@) emacs/$(notdir $@)

emacs/go-mode.el:
	git clone https://github.com/dominikh/$(notdir $@) emacs/$(notdir $@)

emacs/dockerfile-mode:
	git clone git@github.com:masonkatz/$(notdir $@) emacs/$(notdir $@)

emacs/yaml-mode:
	git clone https://github.com/yoshiki/$(notdir $@) emacs/$(notdir $@)

emacs/tramp-term.el:
	git clone https://github.com/randymorris/$(notdir $@) emacs/$(notdir $@)

emacs/web-mode:
	git clone https://github.com/fxbois/$(notdir $@) emacs/$(notdir $@)

emacs/prettier-emacs:
	git clone https://github.com/prettier/$(notdir $@) emacs/$(notdir $@)


##
## main
##

install: install-git install-zsh install-bash install-emacs ## install entire environment
ifeq ($(OS),macosx)
	[ -d ~/.ssh ] || mkdir -m700 .ssh
	ln -s $(pwd)/ssh-config	~/.ssh/config
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
	$(MAKE) clean install PERSONA=$@

softiron: ## reset for softiron config
	$(MAKE) clean install PERSONA=$@

personal: ## reset for personal config
	$(MAKE) clean install

