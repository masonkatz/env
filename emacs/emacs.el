;;; emacs.el - main emacs config file shared across all OSes

(add-to-list 'custom-theme-load-path "~/emacs/themes/")

(setq load-path (cons "~/emacs" load-path))

(if (display-graphic-p)
    (progn
      (tool-bar-mode 0))
  (progn
    (load-theme `zenburn t)))


;; melpa
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)



;; mac keyboard
(global-set-key "\M-[h" (lambda () (interactive) (beginning-of-line 'nil)))
(global-set-key "\M-[f" (lambda () (interactive) (end-of-line 'nil)))

(require 'uniquify) 
(require 'column-marker)

(setq global-font-lock-mode 1)
(server-start)
(display-time-mode 1)
(menu-bar-mode -99)
(column-number-mode)

(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")

;;
;; Programming Mode Settings
;;


(defun global-code-mode ()
  "Common settings for all programming modes.

- turn on line numbers
"
  (interactive)
  (if (display-graphic-p)
      (progn
	(linum-mode 1)))
  (column-marker-1 80))


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
	     (setq py-indent-offset 8)
	     (setq py-indent-paren-spanned-multilines-p 't)
	     (setq python-indent 8)
	     (setq tab-width 8)
	     (global-code-mode)))

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

(add-hook 'sgml-mode-hook
	  '(lambda ()
	     (setq sgml-basic-offset 8)
	     (global-code-mode)))




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
