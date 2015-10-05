;;; Package -- Matt Russell's custom emacs setup -*- lexical-binding: t; coding: utf-8; mode: lisp -*-
;;;
;;; Commentary:
;;;    Integrates with netsight-emacs.
;;; Code:

(defvar user-lisp-directory (expand-file-name "~/elisp")
  "Place to load local LISP code from.")

(defun add-to-hooks (function mode-hooks)
  "Add FUNCTION to multiple modes MODE-HOOKS."
  (mapc (lambda (hook) (add-hook hook function)) mode-hooks))

(use-package company-jedi)

;; Keypress frequency tracking.
(use-package keyfreq)

;; Makes working with data structures easier
(use-package dash)

;; Makes working with filesystem awesome
(use-package f)

;; Makes working with strings awesome
(use-package s)

(use-package flycheck
  :preface

  (defun mattr/flycheck-mode-line-with-checker-name (oldfun &optional status)
    "Show the current checker name in the flycheck mode-line."
    (let ((res (apply oldfun status)))
      ;; Unless there is no current checker
      (if flycheck-checker
	  (s-replace "FlyC" (format "FlyC[%s]" flycheck-checker) res)
	res)))

  (defun mattr/remember-flycheck-checker (checker)
      "Remember the last set CHECKER which should be the same as `flycheck-checker'."
    (pyautomagic--remember-flycheck-checker checker))
  :config
  (advice-add #'flycheck-select-checker
	      :after #'mattr/remember-flycheck-checker)
  (advice-add #'flycheck-mode-line-status-text
	      :around #'mattr/flycheck-mode-line-with-checker-name)
  (flycheck-color-mode-line-mode 1))

;; Used for constributing 3rd party python packages
;; instead of the more imposing flycheck-flake8 checker
;;; (which is the default for my own and work packages)
(use-package flycheck-pyflakes)

(use-package keyfreq)

(use-package netsight
  :config
  (bind-key "C-c t" #'tool-bar-mode)
  (bind-key "C-x 4 s" #'netsight-sudo-edit netsight-keymap)
  ;; avoid audio beeping by turning on visible-bell
  (setq visible-bell t)
  (setq debug-on-error t)
  (setq custom-theme-directory (locate-user-emacs-file "themes"))
  (setq custom-theme-allow-multiple-selections nil)
  (setq-default theme-load-from-file t)
  (setq user-full-name "Matt Russell")
  (add-to-list 'auto-mode-alist '("Makfile.*" . makefile-gmake-mode))
  (keyfreq-mode)
  (menu-bar-mode 0)
  (helm-mode 1)
  (load-theme #'abyss t)
  (powerline-default-theme))

(use-package org
  :preface
  (defun mattr/org-use-speed-commands-for-headings-and-lists ()
    "Activate speed commands on list items too."
    (or (and (looking-at org-outline-regexp) (looking-back "^\**"))
	(save-excursion (and (looking-at (org-item-re)) (looking-back "^[ \t]*")))))
  (defun mattr/org-mode-ask-effort ()
    "Ask for an effort estimate when clocking in."
    (unless (org-entry-get (point) "Effort")
      (let ((effort
	     (completing-read
              "Effort: "
              (org-entry-get-multivalued-property (point) "Effort"))))
	(unless (equal effort "")
	  (org-set-property "Effort" effort)))))
  :config
  (setq org-clock-persist t)
  (setq org-log-done #'time)
  (setq org-todo-keywords
        (quote ((sequence
		 "TODO(t)"
		 "NEXT(n)"
		 "STARTED(s)"
		 "|"
		 "DONE(d)")
		(sequence
		 "WAITING(w@/!)"
		 "HOLD(h@/!)"
		 "|"
		 "CANCELLED(c@/!)"
		 "PHONE"
		 "MEETING"))))
  (setq org-default-notes-file "~/org/notes.org")
  (setq org-show-notification-handler 'message)
  (setq org-agenda-files
	(f-entries "~/org" (apply-partially #'s-ends-with? ".org") t))
  (setq org-directory "~/org")
  (setq org-default-notes-file "~/org/refile.org")
  (setq org-use-effective-time t)
  (setq org-clock-in-switch-to-state "STARTED")
  (setq org-clock-report-include-clocking-task t)
  (setq org-use-speed-commands 'mattr/org-use-speed-commands-for-headings-and-lists)
  (setq org-goto-interface 'outline org-goto-max-level 10)
  (setq org-startup-folded nil)
  (setq org-cycle-include-plain-lists 'integrate)
  (add-to-list 'org-speed-commands-user '("x" org-todo "DONE"))
  (add-to-list 'org-speed-commands-user '("y" org-todo-yesterday "DONE"))
  (add-to-list 'org-speed-commands-user '("!" my/org-clock-in-and-track))
  (add-to-list 'org-speed-commands-user '("s" call-interactively 'org-schedule))
  (add-to-list 'org-speed-commands-user '("d" my/org-move-line-to-destination))
  (add-to-list 'org-speed-commands-user '("i" call-interactively 'org-clock-in))
  (add-to-list 'org-speed-commands-user '("o" call-interactively 'org-clock-out))
  (add-to-list 'org-speed-commands-user '("$" call-interactively 'org-archive-subtree))
  ;; (bind-key "!" 'my/org-clock-in-and-track org-agenda-mode-map)
  (bind-key "C-c j" 'org-clock-goto) ;; jump to current task from anywhere
  (bind-key "C-c C-w" 'org-refile)
  (bind-key "C-c r" 'org-capture)
  (bind-key "C-c a" 'org-agenda)
  (bind-key "C-c l" 'org-store-link)
  (bind-key "C-c L" 'org-insert-link-global)
  (bind-key "C-c O" 'org-open-at-point-global)
  ;; (bind-key "<f9> <f9>" 'org-agenda-list)
  ;; (bind-key "<f9> <f8>" (lambda () (interactive) (org-capture nil "r")))
  (bind-key "C-TAB" 'org-cycle org-mode-map)
  (bind-key "C-c v" 'org-show-todo-tree org-mode-map)
  (bind-key "C-c C-r" 'org-refile org-mode-map)
  (bind-key "C-c R" 'org-reveal org-mode-map)
  (org-clock-persistence-insinuate)
  (org-babel-do-load-languages
   #'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)))
  (eval-after-load 'org-agenda
    '(bind-key "i" 'org-agenda-clock-in org-agenda-mode-map))
  (add-hook 'org-clock-in-prepare-hook 'mattr/org-mode-ask-effort))

(use-package paredit
  :diminish paredit-mode
  :config
  (add-to-hooks #'enable-paredit-mode `(lisp-mode-hook emacs-lisp-mode-hook)))

(use-package powerline
  :config
  (setq-default powerline-default-separator 'wave))


(use-package pretty-symbols
  :diminish pretty-symbols-mode
  :preface
  (defun enable-pretty-symbols-mode ()
    (pretty-symbols-mode 1))
  :config
  (add-to-hooks #'enable-pretty-symbols-mode
		`(emacs-lisp-mode-hook
		  lisp-mode-hook
		  python-mode-hook)))

(use-package frame-cmds
  :bind (("C-c f m" . maximize-frame)
	 ("C-c f r" . restore-frame)
	 ("C-c f o" . other-window-or-frame)
	 ("<M-up>" . move-frame-up)
	 ("<M-down>" . move-frame-down)
	 ("<M-left>" . move-frame-left)
 	 ("<M-right>" . move-frame-right)))

(use-package gnus
  :bind (("C-x g" . gnus-other-frame)))

(use-package dired
  :config
  (setq-default dired-omit-files-p t)
  :init
  (require 'dired-x))

(use-package text-scale-mode
  :bind (("C-c +" . text-scale-increase)
	 ("C-c -" . text-scale-decrease)))

(use-package ispell
  :bind (("C-c i" . ispell-buffer))
  :init
  (add-to-hooks #'flyspell-mode
		`(git-commit-mode-hook
		  jabber-chat-mode-hook
		  rst-mode-hook
		  sphinx-doc-mode-hook)))

(use-package magit
  :bind (("C-c m" . magit-status)))

(use-package jabber
  :load-path user-lisp-directory
  :preface
  (defun set-jabber-credentials ()
    "Reads jabber credentials from encrypted authinfo GPG file.

         Assumptions:

         * Pre-existance of a line such as the following in ~/.authinfo.gpg:
           machine jabber port xmpp login <user-mail-address> password <passwd

         * This is the netsight.co.uk jabber server.

         * Environment variable `EMAIL` is set to a Netsight email address.

     References:
     http://enthusiasm.cozy.org/archives/2014/07/auth-source-getting-my-secrets-out-of-my-emacs-init-file
     https://github.com/ardumont/org/blob/master/articles/emacs-jabber.org"
    (setq creds (auth-source-search :user user-mail-address
				    :port "xmpp"
				    :max 1
				    :require '(:secret)))
    (if creds
	(let* ((authinfo-get (apply-partially #'plist-get (car creds)))
	       (user
		(s-join "/" (list (funcall authinfo-get :user) (system-name))))
	       (host (concat "jabber." (jabber-jid-server user-mail-address)))
	       (port (funcall authinfo-get :port))
	       (passwd (funcall (funcall authinfo-get :secret))))
	(setq jabber-account-list
	      `((,user (:password . ,passwd) (:connection-type . starttls)))))
      (error "Could not read authinfo credentials for Jabber")))
  :config
  (setq-default jabber-avatar-cache-directory "~/.jabber-avatars")
  (setq-default jabber-debug-keep-process-buffers t)
  (add-hook 'after-init-hook #'set-jabber-credentials))

(use-package erc
  :preface
  (defmacro erc-bouncer-connect (command server port nick ssl pass)
   "Create interactive command COMMAND, for connecting to an IRC server.

    Connect to SERVER at PORT using NICK SSL and PASS then issue COMMAND.
    The command uses interactive mode if passed an argument."
   (fset command
         `(lambda (arg)
           (interactive "p")
	   (if (not (= 1 arg))
	       (call-interactively 'erc)
	     (let ((erc-connect-function ',(if ssl
	 				       'erc-open-ssl-stream
					     'open-network-stream)))
 	       (erc :server ,server :port ,port :nick ,nick :password ,pass))))))
  :config
  (autoload 'erc "erc" "" t)
  (erc-bouncer-connect erc-ifs "t4nk.irc.tf" 6697 "mattr" t "637094"))

(use-package recentf
  :bind (("C-x r e" . recentf-edit-list)))

(use-package helm :diminish helm-mode)

(use-package helm-config
  :bind (("C-c h" . helm-command-prefix)
	 ("C-x b" . helm-mini)
	 ("C-x f" . helm-find-files)
	 ("C-x C-r" . helm-recentf)
	 ("M-x" . helm-M-x))
  :preface
  (progn
    (require 'helm)
    (unbind-key "C-x c")
    (bind-key "<tab>" #'helm-execute-persistent-action helm-map)
    (bind-key "C-e" #'recentf-edit-list helm-map)
    (bind-key "C-z" #'helm-select-action helm-map))
  :config
  ;; open helm buffer inside current window, not occupy whole other window
  (setq helm-split-window-in-side-p t)
  ;; move to end or beginning of source when reaching top or bottom of source.
  (setq helm-move-to-line-cycle-in-source t)
  ;; search for library in `require' and `declare-function' sexp.
  (setq helm-ff-search-library-in-sexp t)
  ;; scroll 8 lines other window using M-<next>/M-<prior>s
  (setq helm-scroll-amount 8)
  (setq helm-M-x-fuzzy-match t
	helm-buffers-fuzzy-matching t
	helm-recentf-fuzzy-match t)
  (setq helm-ff-file-name-history-use-recentf t)
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))
  (when (executable-find "ack-grep")
    (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
          helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f")))

(use-package package
  :bind (("C-c C-l" . list-packages)))

(use-package thememgr
  :load-path user-lisp-directory)

(use-package gnus
  :bind (("C-c C-x m" . gnus)))

(use-package reload-dir-locals
  :load-path user-lisp-directory
  :bind (("C-c d l r" . reload-dir-locals-for-current-buffer)))

(use-package pyvenv
  :bind (("C-c w" . pyvenv-workon)
	 ("C-c v a" . pyvenv-activate)
	 ("C-c v d" . pyvenv-deactivate))
  :config
  (add-to-list 'pyvenv-post-activate-hooks #'pyvenv-restart-python)
  :init
  (setq pyvenv-mode-line-indicator
	'(pyvenv-virtual-env-name
	  ("[venv:" pyvenv-virtual-env-name "] "))))

(use-package python
  :bind (("RET" . newline-and-indent))
  :init
  (add-hook #'python-mode-hook
	    (lambda ()
	      (require 'pyautomagic)
	      (setq import-python-el-settings 't)
	      (pyvenv-mode 1)
	      (pyautomagic--flake8-for-current-git-repo)
	      (pyautomagic--venv-for-current-git-repo))))

(use-package jedi
  :config
  (jedi:ac-setup)
  (setq jedi:complete-on-dot 't)
  (bind-key "." #'jedi:goto-definition-pop-marker esc-map)
  (bind-key "S-." #'jedi:goto-definition-push-marker esc-map))

(use-package rst
  :init
  (auto-fill-mode t)
  (pyvenv-mode 1))

(use-package pyautomagic
  :load-path user-lisp-directory
  :bind (("C-c v e" . pyautomagic--activate-venv-safely)
	 ("C-c f c" . pyautomagic--configure-flycheck-checkers)))

(provide '.emacs-custom)
;;; .emacs-custom.el ends here
