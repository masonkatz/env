;;; emacs.el - main emacs config file shared across all OSes

(add-to-list 'load-path "~/emacs")

(require 'package)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(require 'column-marker)
(require 'dockerfile-mode)
(require 'exec-path-from-shell)
(require 'go-mode)
(require 'lsp-mode)
(require 'prettier-js)
(require 'python-mode)
(require 'server)
(require 'tramp-term)
(require 'uniquify) 
(require 'web-mode)
(require 'yaml-mode)


(defvar mjk/dark-theme 'zenburn
  "Dark mode theme")

(defvar mjk/light-theme 'solarized-light
  "Light mode theme - used for printing")

(defvar mjk/bad-go (if (getenv "BADGO")
		     't
		   nil)
  "Use non-standard Go conventions")

(load-theme mjk/dark-theme t)

(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))

(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

(if (display-graphic-p)
    (progn
      (tool-bar-mode 0)
      (setq default-directory "~/")
      (exec-path-from-shell-initialize)
      (set-face-attribute 'default nil :family "JetBrains Mono" :height 140))
  (progn
    (menu-bar-mode -99)))

(setq inhibit-startup-screen t)


(defun ps-print-landscape ()
  "Landscape print current buffer"
  (interactive) 
  (let ((ps-font-size '(8 . 10))
	(ps-line-number t)
	(ps-landscape-mode t)
	(ps-number-of-columns 1)
	(pretty prettify-symbols-mode))
    (if pretty
	(prettify-symbols-mode))
    (load-theme mjk/light-theme t)
;    (ps-print-buffer-with-faces)
    (disable-theme mjk/light-theme)
    (if pretty
	(prettify-symbols-mode))))

(eval-after-load "term"
  '(define-key term-raw-map (kbd "C-c C-y") 'term-paste))

(defun mjk/visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
      (progn
        (ansi-term (getenv "SHELL")))
    (switch-to-buffer "*ansi-term*")))

(global-set-key [f1] 'mjk/visit-term-buffer)
(global-set-key "\M-[h" (lambda () (interactive) (beginning-of-line 'nil)))
(global-set-key "\M-[f" (lambda () (interactive) (end-of-line 'nil)))



(put 'downcase-region 'disabled nil)

(setq display-time-mail-string "")
(column-number-mode)

(if (not (server-running-p))
    (server-start))


(setq uniquify-buffer-name-style 'post-forward uniquify-separator ":")

;;
;; Programming Mode Settings
;;

(defun mjk/use-ligatures ()
  (if (find-font (font-spec :name "JetBrains Mono"))
      (let ((ligatures `(
			 (?-  . ,(regexp-opt '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>" "-->")))
			 (?<  . ,(regexp-opt '("<-" "<<-" "<=>" "<=" "<|" "<||" "<|||::=" "<|>" "<:" "<>" "<-<"
					       "<<<" "<==" "<<=" "<=<" "<==>" "<-|" "<<" "<~>" "<=|" "<~~" "<~"
					       "<$>" "<$" "<+>" "<+" "</>" "</" "<*" "<*>" "<->" "<!--")))
			 (?:  . ,(regexp-opt '(":>" ":<" ":::" "::" ":?" ":?>" ":=")))
			 (?=  . ,(regexp-opt '("=>>" "==>" "=/=" "=!=" "=>" "===" "=:=" "==")))
			 (?!  . ,(regexp-opt '("!==" "!!" "!=")))
			 (?>  . ,(regexp-opt '(">]" ">:" ">>-" ">>=" ">=>" ">>>" ">-" ">=")))
			 (?&  . ,(regexp-opt '("&&&" "&&")))
			 (?|  . ,(regexp-opt '("|||>" "||>" "|>" "|]" "|}" "|=>" "|->" "|=" "||-" "|-" "||=" "||")))
			 (?.  . ,(regexp-opt '(".." ".?" ".=" ".-" "..<" "...")))
			 (?+  . ,(regexp-opt '("+++" "+>" "++")))
			 (?\[ . ,(regexp-opt '("[||]" "[<" "[|")))
			 (?\{ . ,(regexp-opt '("{|")))
			 (?\? . ,(regexp-opt '("??" "?." "?=" "?:")))
			 (?#  . ,(regexp-opt '("####" "###" "#[" "#{" "#=" "#!" "#:" "#_(" "#_" "#?" "#(" "##")))
			 (?\; . ,(regexp-opt '(";;")))
			 (?_  . ,(regexp-opt '("_|_" "__")))
			 (?\\ . ,(regexp-opt '("\\" "\\/")))
			 (?~  . ,(regexp-opt '("~~" "~~>" "~>" "~=" "~-" "~@")))
			 (?$  . ,(regexp-opt '("$>")))
			 (?^  . ,(regexp-opt '("^=")))
			 (?\] . ,(regexp-opt '("]#")))
			 )))
	(dolist (char-regexp ligatures)
	  (set-char-table-range composition-function-table (car char-regexp)
				`([,(cdr char-regexp) 0 font-shape-gstring]))))))
  
(defun mjk/code-mode ()
  (interactive)
  (if (display-graphic-p)
      (progn
	(mjk/use-ligatures)
	(linum-mode 1)))
  (prettify-symbols-mode)
  (show-paren-mode)
  (electric-pair-mode 1)
  (font-lock-mode))


;(add-hook 'before-make-frame-hook 'graphic-setup)

(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook
	  '(lambda ()
	     (if mjk/bad-go
		 (setq tab-width 4
		       indent-tabs-mode nil))
	     (add-hook 'before-save-hook '(lambda ()
					    (if (not mjk/bad-go)
						(gofmt-before-save))))
	     (setq tab-width 4
		   gofmt-command "goimports")
	     (mjk/code-mode)))

(add-hook 'js-mode-hook
	  '(lambda ()
	     (setq js-indent-level 8)   
	     (mjk/code-mode)))

(setq c-default-style "linux")
(add-hook 'c-mode-hook
	  '(lambda ()
	     (setq c-basic-offset 4
		   tab-width 4)
	     (mjk/code-mode)))

(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
	     (mjk/code-mode)))


(add-hook 'ruby-mode-hook
	  '(lambda ()
	     (setq ruby-indent-level 8)
	     (mjk/code-mode)))

(add-hook 'css-mode-hook
	  '(lambda ()
	     (setq css-indent-offset 8)
	     (mjk/code-mode)))

(add-hook 'python-mode-hook
	  '(lambda ()
	     (setq tab-width 8)
	     (setq python-indent 8)
	     (setq py-indent-tabs-mode t)
	     (setq py-indent-offset 8)
	     (setq py-indent-paren-spanned-multilines-p nil)
	     (setq py-closing-list-dedents-bos nil)
	     (mjk/code-mode)))

(add-hook 'nxml-mode-hook
	  '(lambda ()
	     (setq nxml-child-indent 8)))

(add-hook 'sh-mode-hook
	  '(lambda ()
	     (setq sh-basic-offset 8
		   sh-indentation  8
		   sh-indent-for-case-label 0
		   sh-indent-for-case-alt '+)
	     (mjk/code-mode)))

(add-hook 'web-mode-hook
	  '(lambda ()
	     (if (equal web-mode-content-type "javascript")
		 (web-mode-set-content-type "jsx")) ;; react
	     (setq web-mode-markup-indent-offset 2
		   web-mode-css-indent-offset 2
		   web-mode-code-indent-offset 2)
	     (prettier-js-mode)
	     (mjk/code-mode)))


(add-hook 'html-helper-mode-hook
	  '(lambda ()
	     (setq html-helper-basic-offset 8)
	     (mjk/code-mode)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-header-scaling t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
