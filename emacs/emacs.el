;;; emacs.el --- top level config -*- eval: (outshine-cycle-buffer 2) -*-
;;; Commentary:
;;
;; Portable across MacOS and Linux, but assumes a fairly recent version of EMACS.
;;
;;; Code:

;;;; Bootstrap straight.el and use-package

;; bootstrap for straight package manager
;; https://github.com/raxod502/straight.el#getting-started
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;;;; Startup


(use-package server
  :config
  (when (not (server-running-p))
    (server-start)))

(use-package exec-path-from-shell
  :straight t
  :if (memq window-system '(mac ns))
  :config
  (when (display-graphic-p)
    (exec-path-from-shell-initialize)))


(when (display-graphic-p)
  (setq default-directory "~/"))

(when (memq window-system '(mac ns))
  (unbind-key "s-t")
  (unbind-key "s-,"))

;;;; Font, Icons & Ligatures

(use-package all-the-icons
  :straight t)

(use-package all-the-icons-dired
  :straight t
  :after all-the-icons
  :hook
  (dired-mode . all-the-icons-dired-mode))

(defvar mjk/resolution-font-size-alist '(((1280 800)  . 14)
					 ((1440 900)  . 14)
					 ((1680 1050) . 14)
					 ((1920 1080) . 14)
					 ((2560 1440) . 16)
					 ((3008 1692) . 16)
					 ((3360 1890) . 16)
					 ((3440 1440) . 16)
					 ((3840 2160) . 16))
  "Font sizes for different monitors.")

(defun mjk/font-size ()
  "Return font size to use based on resolution."
    (let* ((geometry (cdr (assoc 'geometry (car (display-monitor-attributes-list)))))
	   (resolution (cddr geometry)))
      (* 10 (cdr (assoc (cddr geometry) mjk/resolution-font-size-alist)))))

(when (display-graphic-p)
  (window-divider-mode)
  (set-face-attribute 'default nil :height (mjk/font-size)))

;;;; Modeline

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-project-detection 'projectile
	doom-modeline-buffer-encoding nil
	doom-modeline-indent-info t)
  (size-indication-mode))


;;;; Dark/Light Modes

(defvar mjk/dark-mode-p nil
  "Are we dark or light.")

(defvar mjk/dark-theme 'doom-zenburn
  "Dark mode theme.")

(defvar mjk/light-theme 'doom-solarized-light
  "Light mode theme - used for printing.")

(defun mjk/dark-mode ()
  "Toggle dark/light mode."
  (interactive)
  (if (string= mjk/dark-mode-p t)
      (mjk/force-light-mode)
    (mjk/force-dark-mode)))

(defun mjk/force-dark-mode ()
  "Force dark mode."
  (when (not (string= mjk/dark-mode-p t))
    (when (custom-theme-p mjk/light-theme)
      (disable-theme mjk/light-theme))
    (load-theme mjk/dark-theme t)
    (setq mjk/dark-mode-p t)))

(defun mjk/force-light-mode ()
  "Force light mode."
  (when (string= mjk/dark-mode-p t)
    (when (custom-theme-p mjk/dark-theme)
      (disable-theme mjk/dark-theme))
    (load-theme mjk/light-theme t)
    (setq mjk/dark-mode-p nil)))

(global-set-key (kbd "C-c d")  'mjk/dark-mode)

(use-package doom-themes
  :straight t
  :custom
  (doom-zenburn-brighter-modeline t)
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
;  (doom-themes-treemacs-theme "doom-colors")
  :config
  (doom-themes-visual-bell-config)
;  (doom-themes-treemacs-config)
  (doom-themes-org-config))


(mjk/dark-mode) ;; do it

;;;; Printing

(defun mjk/print-landscape ()
  "Landscape print current buffer."
  (interactive)
  (let ((ps-font-size '(8 . 10))
	(ps-line-number t)
	(ps-landscape-mode t)
	(ps-number-of-columns 1)
	(ligatures ligature-mode)
	(pretty prettify-symbols-mode)
	(dark mjk/dark-mode-p))
    (when pretty (prettify-symbols-mode))
    (when ligatures (ligature-mode))
    (mjk/force-light-mode)
    (ps-print-buffer-with-faces)
    (when dark (mjk/force-dark-mode))
    (when ligatures (ligature-mode))
    (when pretty (prettify-symbols-mode))))

;;;; Outlining

(defun mjk/org-capture-cond (s)
  "Used by org-capture-templates to only include a line for S if non nil."
  (if (= 0 (length s))
      nil
    "  %s\n" s))

(use-package outshine
  :straight t
  :bind (:map outshine-mode-map ("<tab>" . outshine-kbd-TAB))
  :hook
  (prog-mode . outshine-mode))

(use-package org
  :bind
  (:map global-map ("C-c o" . org-capture))
  :config
  (when (string= system-type 'darwin)	; has icloud
    (setq org-directory "~/Documents/Org"))
  (setq org-capture-templates
	'(("t" "Todo" entry (file+headline "Tasks.org" "Backlog")
           "* TODO %?\n%(mjk/org-capture-cond \"%i\")  %u"))))

(use-package org-superstar
  :straight t
  :after (org)
  :hook
  (org-mode . (lambda ()
		(org-superstar-mode 1)
		(push '("[ ]" . "☐") prettify-symbols-alist)
		(push '("[X]" . "☑") prettify-symbols-alist)
		(push '("[-]" . "❍") prettify-symbols-alist)
		(prettify-symbols-mode))))

(use-package org-projectile
  :straight t
  :after (org)
  :config
  (if (string= system-type 'darwin)	; has icloud
      (setq org-projectile-projects-file "~/Documents/Org/Projects.org")
    (org-projectile-per-project))
  (setq org-projectile-capture-template "* TODO %?\n%(mjk/org-capture-cond \"%i\")  %u\n  %a"
	org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
  (push (org-projectile-project-todo-entry) org-capture-templates))

;;;; Which Key

(use-package which-key
  :straight t
  :init
  (which-key-mode))

;;;; Helm

(use-package helm
  :straight t
  :bind
  (([remap find-file] . helm-find-files)
   ([remap execute-extended-command] . helm-M-x)
   ([remap list-buffers] . helm-buffers-list)
   ([remap switch-to-buffer] . helm-buffers-list))
  (:map helm-map
	;; swap defaults for tab key muscle memory
	("<tab>" . 'helm-execute-persistent-action)
	("C-z" . 'helm-select-action))
  :config
  (helm-mode 1))

(use-package helm-projectile
  :straight t
  :after (helm projectile)
  :init
  (setq projectile-enable-caching t
	projectile-completion-system 'helm)
  :config
  (helm-projectile-on))

(use-package helm-lsp
  :straight t
  :after (helm lsp-mode)
  :commands helm-lsp-workspace-symbol)

(use-package helm-company
  :straight t
  :after (helm company))

;(use-package helm-icons
;  :straight t
;  :config
;  (helm-icons-enable))

;;;; Projectile

(use-package projectile
  :straight t
  :init
  (projectile-mode +1)
  :bind-keymap
  ("C-c P" . projectile-command-map))

;;;; Company

(use-package company
  :straight t
  :bind (:map company-active-map
              ("C-n" . company-select-next-or-abort)
              ("C-p" . company-select-previous-or-abort))
  :config
  (setq company-idle-delay 0.5)
  (global-company-mode t))

(use-package company-quickhelp
  :straight t
  :after (company)
  :config
  (setq company-quickhelp-delay 2))

(use-package company-box
  :straight t
  :hook
  (company-mode . company-box-mode))

;;;; Treemacs

(use-package treemacs
  :straight t
  :bind
  (:map global-map
        ("C-c t" . treemacs-select-window))
  :config
  (setq treemacs-is-never-other-window t))

(use-package treemacs-icons-dired
  :straight t
  :after (treemacs dired)
  :config
  (treemacs-icons-dired-mode))

(use-package treemacs-projectile
  :straight t
  :after (treemacs projectile))

(use-package treemacs-magit
  :straight t
  :after (treemacs magit))
  
;;;; Fix Tab

;; Combination of yas/company/term really messes stuff up, need to
;; define custom tab command

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun mjk/tab ()
  "Tab for all modes."
  (interactive)
  (if (string= major-mode "term-mode")
      (term-send-raw-string "\t")
    (if (minibufferp)
	(minibuffer-complete)
      (if (or (not yas/minor-mode)
              (null (do-yas-expand)))
          (if (and company-mode (check-expansion))
              (company-complete-common)
            (indent-for-tab-command))))))

(setq tab-always-indent 'complete)
;;(global-set-key [tab] 'mjk/tab)

;;;; System Administration
;;;;; Docker

(use-package docker
  :straight t)

;;;;; Terminal

(use-package tramp-term
  :straight t)

(use-package term
  :demand t				; force load to get the keymap
  :bind (:map term-raw-map
	      ("C-c C-y" . term-paste)
	      ("M-x" . nil)))		; M-x works as normal

(use-package esh-mode
  :hook
  (eshell-mode . (lambda ()
		   (define-key eshell-mode-map "\C-p" 'eshell-previous-input)
		   (define-key eshell-mode-map "\C-n" 'eshell-next-input)
		   (setenv "PAGER" "cat")
		   (setenv "EDITOR" "emacsclient")
		   (eshell/alias "ls" "ls -F")
		   (when (file-directory-p "/opt/softiron/share/build")
		     (setenv "SI_BUILD" "/opt/softiron/share/build")))))

(use-package eshell-prompt-extras
  :straight t
  :after (esh-mode)
  :config
  (with-eval-after-load "esh-opt"
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda)))

(defun mjk/ansi-term ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
      (ansi-term (getenv "SHELL"))
    (switch-to-buffer "*ansi-term*")))

(defun mjk/eshell ()
  "Create or visit eshell buffer."
  (interactive)
  (if (not (get-buffer "*eshell*"))
      (eshell)
    (switch-to-buffer "*eshell*")))

(global-set-key (kbd "C-c s")  'mjk/ansi-term)
(global-set-key (kbd "C-c e")  'mjk/eshell)

;;;; Writing

;;;;; Markdown

(use-package markdown-mode
  :straight t
  :custom
  (markdown-header-scaling t))

;;;; Programming

(use-package hl-todo
  :straight t
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

(global-set-key (kbd "C-c c")  'compile)
(global-set-key (kbd "C-c n")  'next-error)
(global-set-key (kbd "C-c p")  'previous-error)

;;;;; Git

(use-package git-gutter-fringe
  :straight t)

(use-package magit
  :straight t
  :bind (("C-x g" . magit-status)))

(use-package magit-gitflow
  :straight t
  :after magit
  :hook (magit-mode-hook . turn-on-magit-gitflow))

;;;;; LSP / Flycheck

(use-package flycheck
  :straight t
  :config
  (global-flycheck-mode))

(use-package lsp-mode
  :straight t)

(use-package lsp-ui
  :straight t
  :after (lsp-mode)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (setq lsp-ui-sideline-delay 0.5
	lsp-ui-doc-delay 0.5
	lsp-ui-doc-enable t
	lsp-ui-doc-include-signature nil
	lsp-ui-doc-position 'top))

(when (string= system-type 'darwin)
  (use-package lsp-sourcekit
    :straight t
    :after (lsp-mode)
    :config
    (setq lsp-sourcekit-executable "/Library/Developer/CommandLineTools/usr/bin/sourcekit-lsp")))

;;;;; Yasnippet

(use-package yasnippet
  :straight t)

(use-package yasnippet-snippets
  :straight t)


;;;;;; all

(use-package rainbow-mode
  :straight t)

(add-hook 'prog-mode-hook
	  '(lambda ()
	     (when (display-graphic-p)
	       (setq linum-format "%5d "
		     display-fill-column-indicator-column 80)
	       (linum-mode 1)
	       (display-fill-column-indicator-mode)
	       (hl-line-mode)
	       (git-gutter-mode)
	       (highlight-indent-guides-mode)
	       (set (make-variable-buffer-local 'x-stretch-cursor) t))
	     (prettify-symbols-mode)
	     (show-paren-mode)
	     (electric-pair-mode 1)
	     (company-quickhelp-mode)
	     (font-lock-mode)
	     (flyspell-prog-mode)
	     (yas-minor-mode)))

;;;;;; ELisp

(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
	     (rainbow-delimiters-mode)))

;;;;;; C/C++

(use-package cc-mode
  :config
  (setq c-default-style "linux")
  :hook
  (c-mode . (lambda ()
	      (setq c-basic-offset 4
		    tab-width 4)
	      (lsp-deferred)
	      (flycheck-add-next-checker 'lsp '(warning . c/c++-cppcheck)))))

;;;;;; Go

(defvar mjk/gosht-p nil
  "Are we standard or non-standard Go. Default is standard (nil).")

(defun mjk/gosht (&optional force)
  "Toggle Go style from standard to non-standard, unless FORCE is true."
  (interactive)
  (when (string= force 't)
    (setq mjk/gosht-p 't))
  (if (string= mjk/gosht-p 't)
      (setq mjk/gosht-p nil)
    (setq mjk/gosht-p 't))
  (mjk/go-setup))

(defun mjk/go-setup ()
  "Setup Go style."
  (if (string= mjk/gosht-p 't)
      (setq indent-tabs-mode nil
	    tabs-width 4)
    (setq indent-tabs-mode 't
	  tab-width 8)))

(use-package go-mode
  :straight t
  :after (lsp-mode)
  :hook ((go-mode . (lambda ()
		      (mjk/go-setup)
		      (setq gofmt-command "goimports"
			    display-fill-column-indicator-column 120)
		      (lsp-deferred)
		      (flycheck-add-next-checker 'lsp '(warning . go-golint))))
	 (before-save . (lambda ()
			  (when (not (string= mjk/gosht-p 't))
			    (gofmt-before-save))))))


;;;;;; Javascript

(use-package web-mode
  :straight t
  :hook
  (web-mode . (lambda ()
		(when (equal web-mode-content-type "javascript")
		  (web-mode-set-content-type "jsx")) ;; react
		(setq web-mode-markup-indent-offset 2
		      web-mode-css-indent-offset 2
		      web-mode-code-indent-offset 2)))
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))))

(use-package prettier-js
  :straight t
  :hook
  ((js-mode   . prettier-js-mode)
   (json-mode . prettier-js-mode)
   (web-mode  . prettier-js-mode)))

;;;;;; Make

(add-to-list 'auto-mode-alist '("[Mm]akefile\\'" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.mk\\'" . makefile-gmake-mode))

;;;;;; Python

(use-package python-mode
  :straight t
  :custom
  (py-empty-line-closes-p t)
  (py-use-font-lock-doc-face-p t)
  (py-auto-complete-p t)
  (py-tab-shifts-region-p t))

;;;;;; Salt

(use-package salt-mode
  :straight t
  :config
  (add-hook 'salt-mode-hook
            (lambda ()
              (flyspell-mode 1))))

;;;;;; Shell

(add-hook 'sh-mode-hook
	  '(lambda ()
	     (setq sh-basic-offset 8
		   sh-indentation  8
		   sh-indent-for-case-label 0
		   sh-indent-for-case-alt '+)))


;;;;;; Yaml

(use-package yaml-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

;;;; Misc


(use-package highlight-indent-guides
  :straight t
  :custom
  (highlight-indent-guides-method 'bitmap)
  (highlight-indent-guides-responsive "top"))

(use-package dimmer
  :straight t
  :custom
  (dimmer-fraction 0.1)
  :config
  (dimmer-mode)
  (dimmer-configure-which-key)
  (dimmer-configure-helm)
  (dimmer-configure-magit)
  (dimmer-configure-company-box))

(use-package ace-window
  :straight t
  :config
  (global-set-key [remap other-window] 'ace-window))

(use-package rainbow-delimiters
  :straight t
  :config
  (global-set-key (kbd "C-c r") 'rainbow-delimiters-mode))

(use-package pomidor
  :straight t
  :config
  (setq pomidor-sound-tick nil
        pomidor-sound-tack nil))


(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward))

(put 'downcase-region 'disabled nil)

;;; Custom

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(display-time-load-average-threshold 10)
 '(display-time-mail-string "")
 '(global-auto-revert-mode t)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(safe-local-variable-values '((eval outshine-cycle-buffer 2)))
 '(scroll-bar-mode nil)
 '(sentence-end-double-space nil)
 '(tool-bar-mode nil)
 '(warning-suppress-types '((use-package) (org)))
 '(world-clock-list
   '(("America/Los_Angeles" "San Diego")
     ("America/Phoenix" "Tucson")
     ("America/Denver" "Santa Fe")
     ("America/Chicago" "Dallas")
     ("America/New_York" "New York")
     ("Etc/UTC" "UTC")
     ("Europe/Berlin" "Berlin")
     ("Europe/Moscow" "Moscow")
     ("Asia/Jerusalem" "Jerusalem")
     ("Asia/Calcutta" "Bangalore")
     ("Australia/Perth" "Perth")
     ("Asia/Tokyo" "Tokyo")))
 '(world-clock-time-format "%a %b %d%t%I:%M %p%t%Z"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:foreground "red" :height 4.0))))
 '(font-lock-keyword-face ((t (:slant italic :weight normal))))
 '(linum ((t (:height 0.75))))
 '(treemacs-root-face ((t (:inherit font-lock-constant-face :underline t :weight bold)))))



