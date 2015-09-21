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

(use-package company-jedi)

(use-package keyfreq)

(use-package my-org
  :load-path user-lisp-directory)

(use-package netsight
  :config
  (bind-key "C-c f m" #'toggle-frame-maximized)
  (bind-key "C-x 4 s" #'netsight-sudo-edit netsight-keymap)
  (setq debug-on-error t)
  (setq tab-width 4)
  (setq tab-stop-list (number-sequence 4 200 4))
  (setq custom-theme-directory (locate-user-emacs-file "themes"))
  (setq custom-theme-allow-multiple-selections nil)
  (setq-default theme-load-from-file t)
  (setq user-full-name "Matt Russell")
  (keyfreq-mode)
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

(use-package gnus
  :bind (("C-x g" . gnus-other-frame)))

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
	       (user (concat (funcall authinfo-get :user) "/workstation"))
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

(use-package jedi
  :config
  (progn
    (setq jedi:complete-on-dot 't)))

(use-package pyvenv
  :bind (("C-c w" . pyvenv-workon)
	 ("C-c v a" . pyvenv-activate)
	 ("C-c v d" . pyvenv-deactivate)
	 ("C-c v e" . pyautomagic--activate-venv-safely))
  :config
  (add-to-list 'pyvenv-post-activate-hooks #'pyvenv-restart-python)
  :init
  (setq pyvenv-mode-line-indicator
	'(pyvenv-virtual-env-name
	  ("[venv:" pyvenv-virtual-env-name "] "))))

(use-package python
  :bind (("RET" . newline-and-indent))
  :config
  (add-to-list #'company-backends #'pet-snake)
  (setq import-python-el-settings 't)
  :init
  (defun pet-snake ()
    "My custom `python-mode-hook'."
    (pyvenv-mode 1)
    (pyautomagic--flake8-for-current-git-repo)
    (pyautomagic--venv-for-current-git-repo))
  (add-hook #'python-mode-hook #'pet-snake))

(use-package jedi
  :bind (("C-c p" . jedi:goto-definition-pop-marker)
	 ("C-c n" . jedi:goto-definition-push-marker))
  :config
  (setq jedi:complete-on-dot t)
  :init
  (jedi:ac-setup))

(use-package rst
  :init
  (auto-fill-mode t)
  (pyvenv-mode 1))

(use-package pyautomagic
  :load-path user-lisp-directory)

(use-package sgml-mode
    :config
    (add-hook 'sgml-mode-hook
	    (lambda ()
            (setq indent-tabs-mode nil))))

(provide '.emacs-custom)
;;; .emacs-custom.el ends here
