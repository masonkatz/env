(require 'package)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)
(mapc 'package-install
      '(company-lsp
	dockerfile-mode
	go-mode
	markdown-mode
	prettier-js
	python-mode
	tramp-term
	web-mode
	yaml-mode))
