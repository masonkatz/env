(require 'package)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)
(mapc 'package-install
      '(company
	company-quickhelp
	docker
	eterm-256color
	exec-path-from-shell
	flycheck
	go-mode
	lsp-mode
	lsp-ui
	magit
	magit-gitflow
	markdown-mode
	prettier-js
	projectile
	python-mode
	solarized-theme
	tramp-term
	treemacs
	treemacs-projectile
	web-mode
	yaml-mode
	yasnippet
	yasnippet-snippets
	zenburn-theme))
