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
(require 'helm)
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

(defvar mjk/resolution-font-size-alist '(((1280 800)  . 14)
					 ((1440 900)  . 14)
					 ((1680 1050) . 14)
					 ((1920 1080) . 14)
					 ((2560 1440) . 16)
					 ((3008 1692) . 16)
					 ((3360 1890) . 16)
					 ((3440 1440) . 16)
					 ((3840 2160) . 16))
  "Font sizes for different monitors")

(defvar mjk/dark-theme 'zenburn
  "Dark mode theme")

(defvar mjk/light-theme 'solarized-light
  "Light mode theme - used for printing")

(defvar mjk/dark-mode-p nil
  "Are we dark or light")

(defvar mjk/bad-go (if (getenv "BADGO")
		     't
		   nil)
  "Use non-standard Go conventions")


(defun mjk/font-size ()
  "Return font size to use based on resolution"
    (let* ((geometry (cdr (assoc 'geometry (car (display-monitor-attributes-list)))))
	   (resolution (cddr geometry)))
      (* 10 (cdr (assoc (cddr geometry) mjk/resolution-font-size-alist)))))

(defun mjk/print-landscape ()
  "Landscape print current buffer"
  (interactive) 
  (let ((ps-font-size '(8 . 10))
	(ps-line-number t)
	(ps-landscape-mode t)
	(ps-number-of-columns 1)
	(pretty prettify-symbols-mode)
	(dark mjk/dark-mode-p))
    (when pretty
      (prettify-symbols-mode))
    (mjk/force-light-mode)
;;    (ps-print-buffer-with-faces)
    (when dark
      (mjk/force-dark-mode))
    (disable-theme mjk/light-theme)
    (when pretty
	(prettify-symbols-mode))))

(defun mjk/dark-mode ()
  "Toggles dark/light mode"
  (interactive)
  (if (string= mjk/dark-mode-p t)
      (mjk/force-light-mode)
    (mjk/force-dark-mode)))

(defun mjk/force-dark-mode ()
  "Force dark mode"
  (when (not (string= mjk/dark-mode-p t))
    (when (not (custom-theme-p mjk/dark-theme))
      (load-theme mjk/dark-theme t))
    (disable-theme mjk/light-theme)
    (enable-theme mjk/dark-theme)
    (setq mjk/dark-mode-p t)))

(defun mjk/force-light-mode ()
  "Force light mode"
  (when (string= mjk/dark-mode-p t)
    (when (not (custom-theme-p mjk/light-theme))
      (load-theme mjk/light-theme t))
    (disable-theme mjk/dark-theme)
    (enable-theme mjk/light-theme)
    (setq mjk/dark-mode-p nil)))

(defun mjk/eshell ()
  "Create or visit eshell buffer"
  (interactive)
  (if (not (get-buffer "*eshell*"))
      (eshell)
    (switch-to-buffer "*eshell*")))

(defun mjk/ansi-term ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
      (ansi-term (getenv "SHELL"))
    (switch-to-buffer "*ansi-term*")))

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

(defun mjk/code-mode ()
  (when (display-graphic-p)
    (linum-mode 1))
  (set (make-variable-buffer-local 'x-stretch-cursor) t)
  (prettify-symbols-mode)
  (show-paren-mode)
  (electric-pair-mode 1)
  (company-mode)
  (company-quickhelp-mode)
  (font-lock-mode)
  (flyspell-prog-mode)
  (yas-minor-mode))


(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))

(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))



(when (not (server-running-p))
  (server-start))

(when (display-graphic-p)
  (setq default-directory "~/")
  (exec-path-from-shell-initialize)
  (set-face-attribute 'default nil :height (mjk/font-size)))

(put 'downcase-region 'disabled nil)
(column-number-mode)
(display-time-mode)
(size-indication-mode)
(projectile-mode +1)
(mjk/dark-mode)



;;(global-set-key [tab] 'mjk/tab)
(global-set-key [f1]  'mjk/ansi-term)
(global-set-key [f2]  'mjk/eshell)

(global-set-key "\M-[h" (lambda () (interactive) (beginning-of-line 'nil)))
(global-set-key "\M-[f" (lambda () (interactive) (end-of-line 'nil)))

(define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
(define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)

(define-key term-raw-map (kbd "C-c C-y") 'term-paste)

(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(define-key term-raw-map (kbd "M-x") 'nil) ; still eval sexps in term-mode

(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap execute-extended-command] 'helm-M-x)
(define-key global-map [remap list-buffers] 'helm-buffers-list)

(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook
	  '(lambda ()
	     (when mjk/bad-go
	       (setq indent-tabs-mode nil))
	     (add-hook 'before-save-hook '(lambda ()
					    (when (not mjk/bad-go)
					      (gofmt-before-save))))
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
	     (setq tab-width 8
		   python-indent 8
		   py-indent-tabs-mode t
		   py-indent-offset 8
		   py-indent-paren-spanned-multilines-p nil
		   py-closing-list-dedents-bos nil)
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

(add-hook 'json-mode-hook
	  '(lambda ()
	     (prettier-js-mode)
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
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(c-default-style "linux")
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
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
 '(fci-rule-color "#383838")
 '(global-auto-revert-mode t)
 '(gofmt-command "goimports")
 '(inhibit-startup-screen t)
 '(initial-frame-alist '((width . 128) (height . 64)))
 '(lsp-ui-doc-delay 0.5)
 '(lsp-ui-doc-enable t)
 '(lsp-ui-doc-include-signature nil)
 '(lsp-ui-doc-position 'bottom)
 '(lsp-ui-sideline-delay 0.5)
 '(make-backup-files nil)
 '(markdown-header-scaling t)
 '(menu-bar-mode nil)
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(package-selected-packages
   '(helm-core helm nov treemacs-projectile lsp-treemacs treemacs magit-gitflow magit docker projectile zenburn-theme yasnippet-snippets yaml-mode web-mode tramp-term solarized-theme python-mode prettier-js lsp-ui go-mode flycheck exec-path-from-shell eterm-256color company-quickhelp company-lsp))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style 'post-forward nil (uniquify))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   '((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3")))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:height 0.75))))
 '(treemacs-root-face ((t (:inherit font-lock-constant-face :underline nil :weight bold :height 1.2)))))


