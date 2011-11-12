;;; emacs.el - main emacs config file shared across all OSes

(setq load-path (cons "~/emacs" load-path))
(setq load-path (cons "~/emacs/org-7.4/lisp" load-path))


(cond ((eq window-system 'x)
       '(lambda ()
	  (require 'zenburn))))

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
  (cond ((eq window-system 'x)
	 (linum-mode 1)))
  (column-marker-1 80))


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
	     (setq python-indent 8)
	     (mjk-code-mode)))

(add-hook 'sh-mode-hook
	  '(lambda ()
	     (setq sh-basic-offset 8
		   sh-indentation  8
		   sh-indent-for-case-label 0
		   sh-indent-for-case-alt '+)
	     (mjk-code-mode)))

(add-hook 'html-mode-hook
	  '(lambda ()
	     (setq html-helper-basic-offset 2)
	     (mjk-code-mode)))

(add-hook 'sgml-mode-hook
	  '(lambda ()
	     (setq sgml-basic-offset 8)
	     (mjk-code-mode)))



;;
;; org-mode settings
;;

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)


(org-babel-do-load-languages 'org-babel-load-languages
			     '((python . t)
			       (dot . t)
			       (emacs-lisp . t)))



(setq org-agenda-include-diary t)

(setq org-agenda-custom-commands
      '(("X" agenda "" 	 nil ("~/Desktop/Critical/AGENDA.html"))
	("Y" todo   ""   nil ("~/Desktop/Eventual/TASKS.html"))
	))


(setq org-capture-templates
     '(("j" "Journal" entry (file+datetree "~/org/journal.org")
	"* %?\n  %i")
      ("t" "Todo" entry (file+headline "~/org/tasks.org" "InBox")
	"* TODO %?\n  %i")))


(add-hook 'org-mode-hook 
	  '(lambda ()
	     (setq org-highest-priority ?1
		   org-default-priority ?2
		   org-lowest-priority ?3)
	     (setq org-src-fontify-natively t)
	     (setq org-hide-leading-stars t)
	     (setq org-hidden-keywords t)
	     (setq org-log-done 'time)
	     (setq org-enforce-todo-dependencies t)
	     (setq org-enforce-todo-checkbox-dependencies t)
	     (setq org-todo-keywords 
		   '((sequence "TODO(t)"  "STARTED(s)" "BLOCKED(b@)"  "|"
			       "DONE(d)" "CANCELED(x)" "DELEGATED(o@)")))
	     (setq org-tag-alist '((:startgroup . nil)
				   ("@errand" . ?E)
				   ("@home" . ?H)
				   ("@office" . ?O)
				   ("@phone" . ?P)
				   (:endgroup . nil)
				   ("admin" . ?a)
				   ("amazon" . ?A)
				   ("buy" . ?b) 
				   ("clustercorp" . ?c)
				   ("dell" . ?d)
				   ("family" . ?f)
				   ("hp" . ?h)
				   ("kayo" . ?k)
				   ("personal" . ?p)
				   ("rocks+" . ?r)
				   ("rocks" . ?R)
				   ("ucsd" . ?u)))
	     (setq org-agenda-files (list
				     "~/org/tasks.org"
				     "~/org/journal.org"
				     "~/org/someday.org"))
	     (setq org-mobile-directory "~/Dropbox/MobileOrg")
	     (turn-on-iimage-mode)))

