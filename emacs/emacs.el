;;; emacs.el - main emacs config file shared across all OSes

(add-to-list 'load-path "~/emacs")
(require 'package)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(require 'company)
(require 'column-marker)
(require 'docker)
(require 'exec-path-from-shell)
(require 'go-mode)
(require 'lsp-mode)
(require 'treemacs)
(require 'treemacs-projectile)
(require 'prettier-js)
(require 'projectile)
(require 'python-mode)
(require 'term)
(require 'server)
(require 'tramp-term)
(require 'uniquify) 
(require 'web-mode)
(require 'yaml-mode)
(require 'yasnippet-snippets)
(require 'magit-gitflow)

(defvar mjk/dark-theme 'zenburn
  "Dark mode theme")

(defvar mjk/light-theme 'solarized-light
  "Light mode theme - used for printing")

(defvar mjk/bad-go (if (getenv "BADGO")
		     't
		   nil)
  "Use non-standard Go conventions")

(load-theme mjk/dark-theme t)

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))

(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

(when (display-graphic-p)
  (setq default-directory "~/")
  (exec-path-from-shell-initialize)
  (when (find-font (font-spec :name "JetBrains Mono"))
    (set-face-attribute 'default nil :family "JetBrains Mono" :height 140)))

(column-number-mode)
(display-time-mode)
(size-indication-mode)
(projectile-mode +1)


(defun ps-print-landscape ()
  "Landscape print current buffer"
  (interactive) 
  (let ((ps-font-size '(8 . 10))
	(ps-line-number t)
	(ps-landscape-mode t)
	(ps-number-of-columns 1)
	(pretty prettify-symbols-mode))
    (when pretty
	(prettify-symbols-mode))
    (load-theme mjk/light-theme t)
    (ps-print-buffer-with-faces)
    (disable-theme mjk/light-theme)
    (when pretty
	(prettify-symbols-mode))))


(defun mjk/visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
      (ansi-term (getenv "SHELL"))
    (switch-to-buffer "*ansi-term*")))

(global-set-key [f1] 'mjk/visit-term-buffer)

(global-set-key "\M-[h" (lambda () (interactive) (beginning-of-line 'nil)))
(global-set-key "\M-[f" (lambda () (interactive) (end-of-line 'nil)))

(define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
(define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)

(define-key term-raw-map (kbd "C-c C-y") 'term-paste)

(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(define-key term-raw-map (kbd "M-x") 'nil) ; still eval sexps in term-mode


(put 'downcase-region 'disabled nil)


(when (not (server-running-p))
  (server-start))


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

;; fix yasnippet/company conflicts

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

(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

(defun mjk/code-mode ()
  (interactive)
  (when (display-graphic-p)
;    (mjk/use-ligatures)
    (linum-mode 1))
  (prettify-symbols-mode)
  (show-paren-mode)
  (electric-pair-mode 1)
  (company-mode)
  (company-quickhelp-mode)
  (font-lock-mode)
  (flyspell-prog-mode)
  (yas-minor-mode))

;(add-hook 'before-make-frame-hook 'graphic-setup)

(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook
	  '(lambda ()
	     (when mjk/bad-go
	       (setq indent-tabs-mode nil))
	     (add-hook 'before-save-hook '(lambda ()
					    (when (not mjk/bad-go)
					      (gofmt-before-save))))
	     (if (and (display-graphic-p) (find-font (font-spec :name "Go Mono")))
		 (progn
		   (setq buffer-face-mode-face '(:family "Go Mono" :height 140))
		   (setq line-spacing 2)
		   (buffer-face-mode))
	       (setq line-spacing 0))
	     (setq tab-width 4)
	     (mjk/code-mode)))



(add-hook 'js-mode-hook
	  '(lambda ()
	     (setq js-indent-level 8)   
	     (mjk/code-mode)))

(add-hook 'c-mode-hook
	  '(lambda ()
	     (setq c-basic-offset 4
		   tab-width 4)
	     (mjk/code-mode)))

(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
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
	     (when (equal web-mode-content-type "javascript")
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

; '(lsp-eldoc-hook nil)
; '(lsp-ui-sideline-ignore-duplicate t)
; '(lsp-ui-sideline-show-hover t)
; '(lsp-ui-sideline-show-symbol nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(abbrev-file-name "~/emacs/abbrevs")
 '(c-default-style "linux")
 '(company-quickhelp-delay 2)
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
 '(gofmt-command "goimports")
 '(inhibit-startup-screen t)
 '(lsp-ui-doc-delay 0.5)
 '(lsp-ui-doc-enable t)
 '(lsp-ui-doc-include-signature nil)
 '(lsp-ui-doc-position 'bottom)
 '(lsp-ui-sideline-delay 0.5)
 '(markdown-header-scaling t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(treemacs-projectile lsp-treemacs treemacs magit-gitflow magit docker projectile zenburn-theme yasnippet-snippets yaml-mode web-mode tramp-term solarized-theme python-mode prettier-js lsp-ui go-mode flycheck exec-path-from-shell eterm-256color company-quickhelp company-lsp))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style 'post-forward nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


