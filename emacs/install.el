(require 'package)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)
(mapc 'package-install
      '(
	docker
	eterm-256color
	markdown-mode
	nov
	prettier-js
	solarized-theme
	tramp-term
	treemacs
	treemacs-projectile
	web-mode
	yaml-mode
	))
