;;; Package -- Matt Russell's custom emacs setup -*- lexical-binding: t; coding: utf-8; mode: lisp -*-
;;;
;;; Commentary:
;;; Integrates with netsight-emacs.
;;; Customisation:
;;;  - Adapts python-mode to work with different project styles,
;;;    notably the Pylons project.
;;;
;;; Code:

(defvar user-lisp-directory (expand-file-name "~/elisp")
  "Place to load local LISP code from.")

(defun add-to-hooks (function mode-hooks)
  "Add FUNCTION to multiple modes MODE-HOOKS."
  (mapc (lambda (hook) (add-hook hook function)) mode-hooks))

(use-package netsight
  :config
  (bind-key "C-x 4 s" #'netsight-sudo-edit netsight-keymap)
  (setq debug-on-error t)
  (setq custom-theme-directory (locate-user-emacs-file "themes"))
  (setq custom-theme-allow-multiple-selections nil)
  (setq-default theme-load-from-file t)
  (setq user-full-name "Matt Russell")
  (menu-bar-mode 0)
  (helm-mode 1))

(use-package paredit
  :diminish paredit-mode
  :config
  (add-to-hooks #'enable-paredit-mode `(lisp-mode-hook emacs-lisp-mode-hook)))

(use-package pretty-symbols
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

(use-package org
  :bind (("C-c l" . org-store-link)
	 ("C-c c" . org-capture)
	 ("C-c a" . org-agenda)
	 ("C-c b" . org-iswitchb))
  :config
  (setq org-log-done #'time)
  (setq org-agenda-files
	(list "~/org/work.org"
	      "~/org/home.org"))
  (org-babel-do-load-languages
   #'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t))))

(use-package dired
  :config
  (setq-default dired-omit-files-p t)
  :init
  (require 'dired-x)
  (dired-omit-mode 1))

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
  :bind (("C-c m" . magit-status))
  :config
  (setq magit-last-seen-setup-instructions "1.4.0"))

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
				    :host "jabber"
				    :port "xmpp"
				    :max 1
				    :require '(:secret)))
    (if creds
	(let* ((authinfo-get (apply-partially #'plist-get (car creds)))
	       (user (funcall authinfo-get :user))
	       (host (concat "jabber." (jabber-jid-server user-mail-address)))
	       (port (funcall authinfo-get :port))
	       (passwd (funcall (funcall authinfo-get :secret))))
	(setq jabber-account-list
	      `((,user
		 (:password . ,passwd)
		 (:connection-type . starttls)))))
      (error "Could not read authinfo credentials for Jabber")))
    :config
    (add-hook 'after-init-hook #'set-jabber-credentials))

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

(use-package jedi
  :config
  (progn
    (setq jedi:complete-on-dot 't)))

(use-package pyvenv
  :bind (("C-c w" . pyvenv-workon)
	 ("C-c v d" . pyvenv-deactivate)
	 ("C-c v e" . pyautomagic--activate-venv-safely))
  :config
  (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name
                                     ("[venv:" pyvenv-virtual-env-name "] "))))

(use-package python
  :bind (("RET" . newline-and-indent))
  :init
  (add-hook #'python-mode-hook
	    (lambda ()
	      (pyvenv-mode 1)
	      (pyautomagic--flake8-for-current-git-repo)
	      (pyautomagic--venv-for-current-git-repo))))

(use-package rst
  :init
  (auto-fill-mode t)
  (pyvenv-mode 1))

(use-package pyautomagic
  :load-path user-lisp-directory)

(provide '.emacs-custom)
;;; .emacs-custom.el ends here
