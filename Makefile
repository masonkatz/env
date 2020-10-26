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


install-git: ~/.gitconfig ## 

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

install-zsh: zsh-install.sh powerlevel10k ## 
	sh ./zsh-install.sh --unattended; rm -f ~/.zshrc
	ln -s $(pwd)/powerlevel10k      ~/.oh-my-zsh/custom/themes/powerlevel10k
	ln -s $(pwd)/zshrc		~/.zshrc
	ln -s $(pwd)/zshrc-macosx	~/.zshrc-macosx
	ln -s $(pwd)/zshrc-linux	~/.zshrc-linux
	ln -s $(pwd)/zprofile		~/.zprofile
	ln -s $(pwd)/zprofile-macosx	~/.zprofile-macosx
	ln -s $(pwd)/p10k.zsh		~/.p10k.zsh

zsh-install.sh:
	curl -o $@ -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh

powerlevel10k:
	git clone https://github.com/romkatv/$@


install-bash: ## 
	ln -s $(pwd)/bashrc		~/.bashrc
	ln -s $(pwd)/bashrc-macosx	~/.bashrc-macosx
	ln -s $(pwd)/bashrc-linux	~/.bashrc-linux
	ln -s $(pwd)/profile		~/.profile
	ln -s $(pwd)/profile-macosx	~/.profile-macosx



install-emacs: python-mode go-mode.el dockerfile-mode yaml-mode tramp-term.el web-mode prettier-emacs ## 
	ln -s $(pwd)/emacs		~/
	ln -s ~/emacs/emacs.el		~/.emacs

python-mode:
	git clone https://gitlab.com/python-mode-devs/$@

go-mode.el:
	git clone https://github.com/dominikh/$@

dockerfile-mode:
	git clone git@github.com:masonkatz/$@

yaml-mode:
	git clone https://github.com/yoshiki/$@

tramp-term.el:
	git clone https://github.com/randymorris/$@

web-mode:
	git clone https://github.com/fxbois/$@

prettier-emacs:
	git clone https://github.com/prettier/$@


install: install-git install-zsh install-bash install-emacs ## install entire environment
	ln -s $(pwd)/dircolors		~/.dircolors
	ln -s $(pwd)/bin		~/
	ln -s $(pwd)/screenrc		~/.screenrc



clean-git: ## 
	rm -f ~/.gitconfig

clean-bash: ## 
	rm -f ~/.bashrc
	rm -f ~/.bashrc-macosx
	rm -f ~/.bashrc-linux
	rm -f ~/.profile
	rm -f ~/.profile-macosx
	rm -f ~/.profile-linux
	rm -f ~/.bash_profile

clean-zsh: ## 
	rm -rf ~/.oh-my-zsh zsh-install.sh
	rm -f ~/.zshrc
	rm -f ~/.zshrc-macosx
	rm -f ~/.zshrc-linux
	rm -f ~/.zprofile
	rm -f ~/.zprofile-macosx
	rm -f ~/.zprofile-linux
	rm -f ~/.p10k.zsh

clean-emacs: ## 
	rm -f ~/emacs
	rm -f ~/.emacs

clean: clean-git clean-bash clean-zsh clean-emacs ## clean out everything (do this first)
	rm -f ~/.dircolors
	rm -f ~/bin
	rm -f ~/.screenrc


