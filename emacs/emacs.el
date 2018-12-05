;;; emacs.el - main emacs config file shared across all OSes

(add-to-list 'custom-theme-load-path "~/emacs/themes/")

(add-to-list 'load-path "~/emacs")



(add-to-list 'load-path "~/emacs/go-mode.el")
(require 'go-mode)
;(autoload 'go-mode "go-mode" nil t)
;(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(add-to-list 'load-path "~/emacs/python-mode")
(setq py-install-directory "~/emacs/python-mode")
(require 'python-mode)

(if (display-graphic-p)
    (progn
      (tool-bar-mode 0)
      (set-face-attribute 'default nil :height 160)
      (load-theme 'zenburn t))
  (progn
    (menu-bar-mode -99)
    (load-theme 'zenburn t)))
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
;    (mapc
;     (lambda (package)
;       (or (package-installed-p package)
;	   (package-install package)))
;     '(markdown-mode go-mode))))

;; nicer windows
(defun my-change-window-divider ()
  (let ((display-table (or buffer-display-table standard-display-table)))
    (set-display-table-slot display-table 5 ?│)
    (set-window-display-table (selected-window) display-table)))

;;(add-hook 'window-configuration-change-hook 'my-change-window-divider)

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



(require 'uniquify) 
(require 'column-marker)

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
  (show-paren-mode)
  (column-marker-2 80))


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

(add-hook 'html-helper-mode-hook
	  '(lambda ()
	     (setq html-helper-basic-offset 8)
	     (global-code-mode)))

(put 'downcase-region 'disabled nil)
