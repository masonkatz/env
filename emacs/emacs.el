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

;;;; Font & Ligatures

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
  (set-face-attribute 'default nil :height (mjk/font-size))
  (when (find-font (font-spec :name "JetBrains Mono"))
    (set-face-attribute 'default nil :family "JetBrains Mono")))

;; ligatures -- full set from JetBrains -- pick your favorite
;;
;; "--" "---" "==" "===" "!=" "!==" "=!=" "=:=" "=/="
;; "<=" ">=" "&&" "&&&" "&=" "++" "+++" "***" ";;"
;; "!!" "??" "?:" "?." "?=" "<:" ":<" ":>" ">:"
;; "<>" "<<<" ">>>" "<<" ">>" "||" "-|" "_|_" "|-"
;; "||-" "|=" "||=" "##" "###" "####" "#{" "#[" "]#"
;; "#(" "#?" "#_" "#_(" "#:" "#!" "#=" "^=" "<$>"
;; "<$" "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</"
;; "</>" "/>" "<!--" "<#--" "-->" "->" "->>" "<<-"
;; "<-" "<=<" "=<<" "<<=" "<==" "<=>" "<==>" "==>"
;; "=>" "=>>" ">=>" ">>=" ">>-" ">-" ">--" "-<" "-<<"
;; ">->" "<-<" "<-|" "<=|" "|=>" "|->" "<->" "<~~"
;; "<~" "<~>" "~~" "~~>" "~>" "~-" "-~" "~@"
;; "[||]" "|]" "[|" "|}" "{|" "[<" ">]" "|>" "<|"
;; "||>" "<||" "|||>" "<|||" "<|>" "..." ".." ".="
;; ".-" "..<" ".?" "::" ":::" ":=" "::=" ":?" ":?>"

(use-package ligature
  :straight
  (ligatures :type git :host github :repo "mickeynp/ligature.el")
  :config
  (ligature-set-ligatures 'prog-mode '("--" "++"
				       "==" "!="
				       "<=" ">="
				       "&&" "&="
				       "||" "|="
				       "##" "###" "####"
				       "#!"
				       "/>" "</>"
				       "<!--" "-->"
				       ".." "..."
				       "::"
				       ":="
				       "//"
				       "/*" "***" "*/"))
  :hook
  (prog-mode . ligature-mode))

;;;; Modeline

(use-package powerline
  :straight t
  :init
  (powerline-default-theme)
  :config
  (column-number-mode)
  (display-time-mode)
  (size-indication-mode))

;;;; Dark/Light Modes

(use-package zenburn-theme :straight t)
(use-package solarized-theme :straight t)

(defvar mjk/dark-mode-p nil
  "Are we dark or light.")

(defvar mjk/dark-theme 'zenburn
  "Dark mode theme.")

(defvar mjk/light-theme 'solarized-light
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
    (when (not (custom-theme-p mjk/dark-theme))
      (load-theme mjk/dark-theme t))
    (disable-theme mjk/light-theme)
    (enable-theme mjk/dark-theme)
    (setq mjk/dark-mode-p t)))

(defun mjk/force-light-mode ()
  "Force light mode."
  (when (string= mjk/dark-mode-p t)
    (when (not (custom-theme-p mjk/light-theme))
      (load-theme mjk/light-theme t))
    (disable-theme mjk/dark-theme)
    (enable-theme mjk/light-theme)
    (setq mjk/dark-mode-p nil)))

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

(use-package outshine
  :straight t
  :bind (:map outshine-mode-map
	      ("<tab>" . outshine-kbd-TAB))
  :hook
  (prog-mode . outshine-mode))

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
   ("C-x b"              . helm-buffers-list))
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

;;;; Projectile

(use-package projectile
  :straight t
  :init
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map))


;;;; Company

(use-package company
  :straight t
  :bind (:map company-active-map
              ("C-n" . company-select-next-or-abort)
              ("C-p" . company-select-previous-or-abort))
  :config
  (setq company-idle-delay 0.3)
  (global-company-mode t))

(use-package company-quickhelp
  :straight t
  :after (company)
  :config
  (setq company-quickhelp-delay 2))

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

(global-set-key [tab] 'mjk/tab)

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
		   (define-key eshell-mode-map "\C-n" 'eshell-next-input))))

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

(global-set-key [f1]  'mjk/ansi-term)
(global-set-key [f2]  'mjk/eshell)

;;;; Writing
;;;;; Flycheck

(use-package flycheck
  :straight t
  :config
  (global-flycheck-mode))

;;;;; Markdown

(use-package markdown-mode
  :straight t
  :custom
  (markdown-header-scaling t))

;;;; Progamming
;;;;; Git

(use-package magit
  :straight t
  :bind (("C-x g" . magit-status)))

(use-package magit-gitflow
  :straight t
  :after magit
  :hook (magit-mode-hook . turn-on-magit-gitflow))

;;;;; LSP

(use-package lsp-mode
  :straight t
  :hook (go-mode . lsp-deferred)
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "gopls")
		    :major-modes '(go-mode)
		    :remote? t
                    :server-id 'gopls-remote)))


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
	lsp-ui-doc-position 'bottom))

;;;;; Yasnippet

(use-package yasnippet
  :straight t)

(use-package yasnippet-snippets
  :straight t)

;;;;; Languages
;;;;;; all

(add-hook 'prog-mode-hook
	  '(lambda ()
	     (when (display-graphic-p)
	       (linum-mode 1))
	     (set (make-variable-buffer-local 'x-stretch-cursor) t)
	     (prettify-symbols-mode)
	     (show-paren-mode)
	     (electric-pair-mode 1)
	     (company-quickhelp-mode)
	     (font-lock-mode)
	     (flyspell-prog-mode)
	     (yas-minor-mode)))

;;;;;; C/C++

(use-package cc-mode
  :config
  (setq c-default-style "linux")
  :hook
  (c-mode . (lambda ()
	      (setq c-basic-offset 4
		    tab-width 4))))

;;;;;; Go

(defvar mjk/bad-go (if (getenv "BADGO")
		     't
		   nil)
  "Use non-standard Go conventions.")

(use-package go-mode
  :straight t
  :hook ((go . (lambda ()
		 (when mjk/bad-go
		   (setq indent-tabs-mode nil))
		 (setq tab-width 4)))
	 (before-save . (lambda ()
			  (when (not mjk/bad-go)
			    (gofmt-before-save)))))
  :config
  (setq gofmt-command "goimports"))

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

;;;;;; Python

(use-package python-mode
  :straight t
  :hook (python-mode . (lambda ()
			 (setq tab-width 8
			       python-indent 8
			       py-indent-tabs-mode t
			       py-indent-offset 8
			       py-indent-paren-spanned-multilines-p nil
			       py-closing-list-dedents-bos nil))))

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
 '(display-time-world-list
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
 '(display-time-world-time-format "%a %b %d%t%I:%M %p%t%Z")
 '(global-auto-revert-mode t)
 '(inhibit-startup-screen t)
 '(initial-frame-alist '((width . 128) (height . 64)))
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(safe-local-variable-values
   '((lsp--override-calculate-lisp-indent\? . t)
     (flycheck-disabled-checkers quote
				 (emacs-lisp-checkdoc))
     (eval progn
	   (let
	       ((dirloc-lsp-defun-regexp
		 (concat
		  (concat "^\\s-*(" "lsp-defun" "\\s-+\\(")
		  (or
		   (bound-and-true-p lisp-mode-symbol-regexp)
		   "\\(?:\\sw\\|\\s_\\|\\\\.\\)+")
		  "\\)")))
	     (add-to-list 'imenu-generic-expression
			  (list "Functions" dirloc-lsp-defun-regexp 1)))
	   (defvar lsp--override-calculate-lisp-indent\? nil "Whether to override `lisp-indent-function' with
              the updated `calculate-lisp-indent' definition from
              Emacs 28.")
	   (defun wrap-calculate-lisp-indent
	       (func &optional parse-start)
	     "Return appropriate indentation for current line as Lisp code.
In usual case returns an integer: the column to indent to.
If the value is nil, that means don't change the indentation
because the line starts inside a string.

PARSE-START may be a buffer position to start parsing from, or a
parse state as returned by calling `parse-partial-sexp' up to the
beginning of the current line.

The value can also be a list of the form (COLUMN CONTAINING-SEXP-START).
This means that following lines at the same level of indentation
should not necessarily be indented the same as this line.
Then COLUMN is the column to indent to, and CONTAINING-SEXP-START
is the buffer position of the start of the containing expression."
	     (if
		 (not lsp--override-calculate-lisp-indent\?)
		 (funcall func parse-start)
	       (save-excursion
		 (beginning-of-line)
		 (let
		     ((indent-point
		       (point))
		      state
		      (desired-indent nil)
		      (retry t)
		      whitespace-after-open-paren calculate-lisp-indent-last-sexp containing-sexp)
		   (cond
		    ((or
		      (markerp parse-start)
		      (integerp parse-start))
		     (goto-char parse-start))
		    ((null parse-start)
		     (beginning-of-defun))
		    (t
		     (setq state parse-start)))
		   (unless state
		     (while
			 (<
			  (point)
			  indent-point)
		       (setq state
			     (parse-partial-sexp
			      (point)
			      indent-point 0))))
		   (while
		       (and retry state
			    (>
			     (elt state 0)
			     0))
		     (setq retry nil)
		     (setq calculate-lisp-indent-last-sexp
			   (elt state 2))
		     (setq containing-sexp
			   (elt state 1))
		     (goto-char
		      (1+ containing-sexp))
		     (if
			 (and calculate-lisp-indent-last-sexp
			      (> calculate-lisp-indent-last-sexp
				 (point)))
			 (let
			     ((peek
			       (parse-partial-sexp calculate-lisp-indent-last-sexp indent-point 0)))
			   (if
			       (setq retry
				     (car
				      (cdr peek)))
			       (setq state peek)))))
		   (if retry nil
		     (goto-char
		      (1+ containing-sexp))
		     (setq whitespace-after-open-paren
			   (looking-at
			    (rx whitespace)))
		     (if
			 (not calculate-lisp-indent-last-sexp)
			 (setq desired-indent
			       (current-column))
		       (parse-partial-sexp
			(point)
			calculate-lisp-indent-last-sexp 0 t)
		       (cond
			((looking-at "\\s("))
			((>
			  (save-excursion
			    (forward-line 1)
			    (point))
			  calculate-lisp-indent-last-sexp)
			 (if
			     (or
			      (=
			       (point)
			       calculate-lisp-indent-last-sexp)
			      whitespace-after-open-paren)
			     nil
			   (progn
			     (forward-sexp 1)
			     (parse-partial-sexp
			      (point)
			      calculate-lisp-indent-last-sexp 0 t)))
			 (backward-prefix-chars))
			(t
			 (goto-char calculate-lisp-indent-last-sexp)
			 (beginning-of-line)
			 (parse-partial-sexp
			  (point)
			  calculate-lisp-indent-last-sexp 0 t)
			 (backward-prefix-chars)))))
		   (let
		       ((normal-indent
			 (current-column)))
		     (cond
		      ((elt state 3)
		       nil)
		      ((and
			(integerp lisp-indent-offset)
			containing-sexp)
		       (goto-char containing-sexp)
		       (+
			(current-column)
			lisp-indent-offset))
		      (calculate-lisp-indent-last-sexp
		       (or
			(and lisp-indent-function
			     (not retry)
			     (funcall lisp-indent-function indent-point state))
			(and
			 (save-excursion
			   (goto-char indent-point)
			   (skip-chars-forward " 	")
			   (looking-at ":"))
			 (save-excursion
			   (goto-char calculate-lisp-indent-last-sexp)
			   (backward-prefix-chars)
			   (while
			       (not
				(or
				 (looking-back "^[ 	]*\\|([ 	]+"
					       (line-beginning-position))
				 (and containing-sexp
				      (>=
				       (1+ containing-sexp)
				       (point)))))
			     (forward-sexp -1)
			     (backward-prefix-chars))
			   (setq calculate-lisp-indent-last-sexp
				 (point)))
			 (> calculate-lisp-indent-last-sexp
			    (save-excursion
			      (goto-char
			       (1+ containing-sexp))
			      (parse-partial-sexp
			       (point)
			       calculate-lisp-indent-last-sexp 0 t)
			      (point)))
			 (let
			     ((parse-sexp-ignore-comments t)
			      indent)
			   (goto-char calculate-lisp-indent-last-sexp)
			   (or
			    (and
			     (looking-at ":")
			     (setq indent
				   (current-column)))
			    (and
			     (<
			      (line-beginning-position)
			      (prog2
				  (backward-sexp)
				  (point)))
			     (looking-at ":")
			     (setq indent
				   (current-column))))
			   indent))
			normal-indent))
		      (desired-indent)
		      (t normal-indent)))))))
	   (when
	       (< emacs-major-version 28)
	     (advice-add #'calculate-lisp-indent :around #'wrap-calculate-lisp-indent)))
     (eval outshine-cycle-buffer 2)
     (eval outshine-cycle-buffer)))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:height 0.75)))))


