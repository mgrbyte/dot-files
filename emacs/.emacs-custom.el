;;; Package -- Matt Russell's custom emacs setup
;;;
;;; Commentary:
;;; Integrates with netsight-emacs.
;;; Customisation:
;;;  - Adapts python-mode to work with different project styles,
;;;    notably the Pylons project.
;;
;;; Code:

(defvar user-lisp-directory (expand-file-name "~/elisp")
  "Place to load local LISP code from.")

(defun add-to-hooks (function mode-hooks)
  "Add FUNCTION to multiple modes MODE-HOOKS."
  (mapc (lambda (hook) (add-hook hook function)) mode-hooks))

(use-package emacs-lisp-mode
  :mode (("*scratch*" . emacs-lisp-mode)
	 ("\\.el$" . emacs-lisp-mode)))

(use-package paredit-mode
  :init
  (add-to-hooks #'enable-paredit-mode `(lisp-mode-hook emacs-lisp-mode-hook)))

(use-package pretty-symbols-mode
  :config
  (add-hooks 'emacs-lisp-mode))
  
(use-package auth-source)

(use-package frame-cmds
  :bind (("C-c f m" . maximize-frame)
	 ("C-c f r" . restore-frame)
	 ("C-c f o" . other-window-or-frame)
	 ("<M-up>" . move-frame-up)
	 ("<M-down>" . move-frame-down)
	 ("<M-left>" . move-frame-left)
 	 ("<M-right>" . move-frame-right)))

t(use-package gnus
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

(use-package helm-config
  :bind (("C-c h" . helm-command-prefix)
	 ("C-x b" . helm-mini)
	 ("C-x f" . helm-find-files)
	 ("M-x" . helm-M-x))
  :preface
  (progn
    (require 'helm)
    (global-unset-key (kbd "C-x c"))
    (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-e") #'recentf-edit-list)
    (define-key helm-map (kbd "C-z") #'helm-select-action))
  :config
  ;; open helm buffer inside current window, not occupy whole other window
  (setq helm-split-window-in-side-p t)
  ;; move to end or beginning of source when reaching top or bottom of source.
  (setq helm-move-to-line-cycle-in-source t)
  ;; search for library in `require' and `declare-function' sexp.
  (setq helm-ff-search-library-in-sexp t)
  ;; scroll 8 lines other window using M-<next>/M-<prior>s
  (setq helm-scroll-amount 8)
  (setq helm-M-x-fuzzy-match te
	helm-buffers-fuzzy-matching t
	helm-recentf-fuzzy-match t)
  (setq helm-ff-file-name-history-use-recentf t)
  :init
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t)))

(use-package pyautomagic
  :load-path user-lisp-directory)

(use-package thememgr
  :load-path user-lisp-directory)

(setq debug-on-error t)
(setq custom-theme-directory (locate-user-emacs-file "themes"))
(setq custom-theme-allow-multiple-selections nil)
(setq-default theme-load-from-file t)
(setq user-full-name "Matt Russell")
(menu-bar-mode 0)
(helm-mode 1)

(provide '.emacs-custom)
;;; .emacs-custom.el ends here
