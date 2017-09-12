;;; emacs.el - main emacs config file shared across all OSes

(add-to-list 'custom-theme-load-path "~/emacs/themes/")

(add-to-list 'load-path "~/emacs")

(add-to-list 'load-path "~/emacs/python-mode")
(setq py-install-directory "~/emacs/python-mode")
(require 'python-mode)

(if (display-graphic-p)
    (progn
      (tool-bar-mode 0))
  (progn
    (menu-bar-mode -99)
    (load-theme 'zenburn t)))


;; For emacs-version >= 24.4 configue the package system and and the
;; desired packages.

(if (and (= emacs-major-version 24)(< emacs-minor-version 4))
    ()
  (progn
    (require 'package)
    (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
			(not (gnutls-available-p))))
	   (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
      (add-to-list 'package-archives (cons "melpa" url) t))
    (package-initialize)
    (mapc
     (lambda (package)
       (or (package-installed-p package)
	   (package-install package)))
     '(magit))))


;; mac keyboard
(global-set-key "\M-[h" (lambda () (interactive) (beginning-of-line 'nil)))
(global-set-key "\M-[f" (lambda () (interactive) (end-of-line 'nil)))

(global-set-key "\C-n" 'next-error)
(global-set-key "\C-p" 'previous-error)

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
	     (py-closing-list-dedents-bos t)
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



