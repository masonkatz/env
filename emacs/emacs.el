;;; emacs.el - main emacs config file shared across all OSes

(setq load-path (cons "~/emacs" load-path))
(setq load-path (cons "~/emacs/org-7.4/lisp" load-path))

(if (display-graphic-p)
    (progn
      (require 'zenburn)
      (zenburn)
      (tool-bar-mode 0)))


;; mac keyboard
(global-set-key "\M-[h" (lambda () (interactive) (beginning-of-line 'nil)))
(global-set-key "\M-[f" (lambda () (interactive) (end-of-line 'nil)))

(require 'uniquify) 
(require 'org)
(require 'hideshow-org)
(require 'column-marker)

(setq global-font-lock-mode 1)
(server-start)

(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")

;;
;; Programming Mode Settings
;;


(defun mjk-code-mode ()
  "Common settings for all programming modes.

- turn on line numbers
- turn on hideshow-org (org-mode like folding)
"
  (interactive)
  (if (display-graphic-p)
      (progn
	(linum-mode 1)))
  (column-marker-1 80))


(add-hook 'js-mode-hook
	  '(lambda ()
	     (setq js-indent-level 8)
	     (mjk-code-mode)))

(add-hook 'c-mode-hook
	  '(lambda ()
	     (setq c-basic-offset 8)
	     (mjk-code-mode)))

(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
	     (mjk-code-mode)))

(add-hook 'ruby-mode-hook
	  '(lambda ()
	     (setq ruby-indent-level 8)
	     (mjk-code-mode)))

(add-hook 'css-mode-hook
	  '(lambda ()
	     (setq css-indent-offset 8)
	     (mjk-code-mode)))

(add-hook 'python-mode-hook
	  '(lambda ()
	     (setq py-indent-offset 8)
	     (setq py-indent-paren-spanned-multilines-p 't)
	     (setq python-indent 8)
	     (setq tab-width 8)
	     (mjk-code-mode)))

(add-hook 'sh-mode-hook
	  '(lambda ()
	     (setq sh-basic-offset 8
		   sh-indentation  8
		   sh-indent-for-case-label 0
		   sh-indent-for-case-alt '+)
	     (mjk-code-mode)))

(add-hook 'html-helper-mode-hook
	  '(lambda ()
	     (setq html-helper-basic-offset 8)
	     (mjk-code-mode)))

(add-hook 'sgml-mode-hook
	  '(lambda ()
	     (setq sgml-basic-offset 8)
	     (mjk-code-mode)))



