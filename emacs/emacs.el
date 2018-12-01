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

;; hyper key

(progn
  (let ((map (if (boundp 'input-decode-map)
                 input-decode-map
	       function-key-map)))
    (define-key map "\e[1;P9"  (kbd "H-a"))
    (define-key map "\e[1;P10" (kbd "H-b"))
    (define-key map "\e[1;P11" (kbd "H-c"))
    (define-key map "\e[1;P12" (kbd "H-d"))
    (define-key map "\e[1;P13" (kbd "H-e"))
    (define-key map "\e[1;P14" (kbd "H-f"))
    (define-key map "\e[1;P15" (kbd "H-g"))
    (define-key map "\e[1;P16" (kbd "H-h"))
    (define-key map "\e[1;P17" (kbd "H-i"))
    (define-key map "\e[1;P18" (kbd "H-j"))
    (define-key map "\e[1;P19" (kbd "H-k"))
    (define-key map "\e[1;P20" (kbd "H-l"))
    (define-key map "\e[1;P21" (kbd "H-m"))
    (define-key map "\e[1;P22" (kbd "H-n"))
    (define-key map "\e[1;P23" (kbd "H-o"))
    (define-key map "\e[1;P24" (kbd "H-p"))
    (define-key map "\e[1;P25" (kbd "H-q"))
    (define-key map "\e[1;P26" (kbd "H-r"))
    (define-key map "\e[1;P27" (kbd "H-s"))
    (define-key map "\e[1;P28" (kbd "H-t"))
    (define-key map "\e[1;P29" (kbd "H-u"))
    (define-key map "\e[1;P30" (kbd "H-v"))
    (define-key map "\e[1;P31" (kbd "H-w"))
    (define-key map "\e[1;P32" (kbd "H-x"))
    (define-key map "\e[1;P33" (kbd "H-y"))
    (define-key map "\e[1;P34" (kbd "H-z"))
    (define-key map "\e[1;P35" (kbd "H-0"))
    (define-key map "\e[1;P36" (kbd "H-1"))
    (define-key map "\e[1;P37" (kbd "H-2"))
    (define-key map "\e[1;P38" (kbd "H-3"))
    (define-key map "\e[1;P39" (kbd "H-4"))
    (define-key map "\e[1;P40" (kbd "H-5"))
    (define-key map "\e[1;P41" (kbd "H-6"))
    (define-key map "\e[1;P42" (kbd "H-7"))
    (define-key map "\e[1;P43" (kbd "H-8"))
    (define-key map "\e[1;P44" (kbd "H-9"))
    (define-key map "\e[1;P45" (kbd "H-<f1>"))
    (define-key map "\e[1;P46" (kbd "H-<f2>"))
    (define-key map "\e[1;P47" (kbd "H-<f3>"))
    (define-key map "\e[1;P48" (kbd "H-<f4>"))
    (define-key map "\e[1;P49" (kbd "H-<f5>"))
    (define-key map "\e[1;P50" (kbd "H-<f6>"))
    (define-key map "\e[1;P51" (kbd "H-<f7>"))
    (define-key map "\e[1;P52" (kbd "H-<f8>"))
    (define-key map "\e[1;P53" (kbd "H-<f9>"))
    (define-key map "\e[1;P54" (kbd "H-<f10>"))
    (define-key map "\e[1;P55" (kbd "H-<f11>"))
    (define-key map "\e[1;P56" (kbd "H-<f12>"))
    (define-key map "\e[1;P57" (kbd "H-<f13>"))
    (define-key map "\e[1;P58" (kbd "H-<f14>"))
    (define-key map "\e[1;P59" (kbd "H-<f15>"))))



(global-set-key "\M-[h" (lambda () (interactive) (beginning-of-line 'nil)))
(global-set-key "\M-[f" (lambda () (interactive) (end-of-line 'nil)))

(global-set-key "\C-n" 'next-error)
(global-set-key "\C-p" 'previous-error)
(global-set-key (kbd "H-t") 'visit-term-buffer)

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

