;;; emacs.el - main emacs config file shared across all OSes
;; ____________________________________________________________________________
;; Aquamacs custom-file warning:
;; Warning: After loading this .emacs file, Aquamacs will also load
;; customizations from `custom-file' (customizations.el). Any settings there
;; will override those made here.
;; Consider moving your startup settings to the Preferences.el file, which
;; is loaded after `custom-file':
;; ~/Library/Preferences/Aquamacs Emacs/Preferences
;; _____________________________________________________________________________
(add-to-list 'custom-theme-load-path "~/emacs/themes/")
(add-to-list 'load-path "~/emacs")

(require 'package)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
;(package-refresh-contents)
(mapc (lambda (package)
	(or (package-installed-p package)
	    (package-install package)))
      '(company-lsp
	dockerfile-mode
	go-mode
	markdown-mode
	prettier-js
	python-mode
	tramp-term
	web-mode
	yaml-mode))

(require 'column-marker)
(require 'company-lsp)
(require 'dockerfile-mode)
(require 'go-mode)
(require 'lsp-mode)
(require 'prettier-js)
(require 'python-mode)
(require 'tramp-term)
(require 'uniquify) 
(require 'web-mode)
(require 'yaml-mode)

(push 'company-lsp company-backends)


(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))

;(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))


;(autoload 'go-mode "go-mode" nil t)

(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

(setq py-install-directory "~/emacs/python-mode")

(defun graphic-setup ()
  (tool-bar-mode 0)
  (set-face-attribute 'default nil :height 180))

(if (display-graphic-p)
    (graphic-setup)
  (progn
    (menu-bar-mode -99)))

(load-theme 'zenburn t)
(setq inhibit-startup-screen t)

;; For emacs-version >= 24.4 configue the package system and and the
;; desired packages.

;(if (and (= emacs-major-version 24)(< emacs-minor-version 4))
;    ()
;  (progn
;    (require 'package)
;    (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
;			(not (gnutls-available-p))))
;	   (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
;      (add-to-list 'package-archives (cons "melpa" url) t))
;    (package-initialize)

(defun visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
      (progn
        (ansi-term (getenv "SHELL")))
    (switch-to-buffer "*ansi-term*")))


(global-set-key [f1] 'visit-term-buffer)
(global-set-key "\M-[h" (lambda () (interactive) (beginning-of-line 'nil)))
(global-set-key "\M-[f" (lambda () (interactive) (end-of-line 'nil)))




(setq global-font-lock-mode 1)
(setq display-time-mail-string "")
(display-time-mode 1)
(column-number-mode)

(server-start)

(setq uniquify-buffer-name-style 'post-forward uniquify-separator ":")

;;
;; Programming Mode Settings
;;


(defun global-code-mode ()
  (interactive)
  (if (display-graphic-p)
      (progn
	(linum-mode 1)))
  (show-paren-mode))


(add-hook 'before-make-frame-hook 'graphic-setup)
	     
(add-hook 'go-mode-hook #'lsp)
(add-hook 'go-mode-hook
	  '(lambda ()
	     (setq tab-width 4)
	     (setq gofmt-command "goimports")
	     (add-hook 'before-save-hook 'gofmt-before-save)))
	     
(add-hook 'js-mode-hook
	  '(lambda ()
	     (setq js-indent-level 8)   
	     (global-code-mode)))

(add-hook 'c-mode-hook
	  '(lambda ()
	     (setq c-basic-offset 8)
	     (global-code-mode)))

(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
	     (global-code-mode)))

(add-hook 'ruby-mode-hook
	  '(lambda ()
	     (setq ruby-indent-level 8)
	     (global-code-mode)))

(add-hook 'css-mode-hook
	  '(lambda ()
	     (setq css-indent-offset 8)
	     (global-code-mode)))

(add-hook 'python-mode-hook
	  '(lambda ()
	     (setq tab-width 8)
	     (setq python-indent 8)
	     (setq py-indent-tabs-mode t)
	     (setq py-indent-offset 8)
	     (setq py-indent-paren-spanned-multilines-p nil)
	     (setq py-closing-list-dedents-bos nil)
	     (global-code-mode)))

(add-hook 'nxml-mode-hook
	  '(lambda ()
	     (setq nxml-child-indent 8)))

(add-hook 'sh-mode-hook
	  '(lambda ()
	     (setq sh-basic-offset 8
		   sh-indentation  8
		   sh-indent-for-case-label 0
		   sh-indent-for-case-alt '+)
	     (global-code-mode)))

(add-hook 'web-mode-hook
	  '(lambda ()
	     (if (equal web-mode-content-type "javascript")
		 (web-mode-set-content-type "jsx")) ;; react
	     (setq web-mode-markup-indent-offset 2
		   web-mode-css-indent-offset 2
		   web-mode-code-indent-offset 2)
	     (prettier-js-mode)
	     (global-code-mode)))


(add-hook 'html-helper-mode-hook
	  '(lambda ()
	     (setq html-helper-basic-offset 8)
	     (global-code-mode)))

(put 'downcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (company-lsp lsp-docker lsp-mode go-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
