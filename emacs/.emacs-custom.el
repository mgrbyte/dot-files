;;; Package -- Matt Russell's custom emacs setup
;;;
;;; Commentary:
;;; Integrates with netsight-emacs.
;;; Customisations:
;;;  - Adapts python-mode to work with differnet project styles,
;;;    notably the Pylons project.
;;
;;; Code:

(defvar user-lisp-directory (expand-file-name "~/elisp"))

(use-package org
  :bind (("C-c l" . org-store-link)
	 ("C-c c" . org-capture)
	 ("C-c a" . org-agenda)
	 ("C-c b" . org-iswitchb))
  :config
  (progn
    (setq org-log-done #'time)
    (setq org-agenda-files
	  (list "~/org/work.org"
		"~/org/home.org"))
    (org-babel-do-load-languages
     #'org-babel-load-languages
     '((emacs-lisp . t)
       (python . t)))))

(use-package dired
  :config
  (progn
    (setq-default dired-omit-files-p t)
    (add-hook #'dired-load-hook
	  '(lambda ()
	     (require 'dired-x)
	     (dired-omit-mode 1)))))

(use-package text-scale-mode
  :bind (("C-c +" . text-scale-increase)
	 ("C-c -" . text-scale-decrease)))
 
(use-package magit
  :bind (("C-c m" . magit-status)))

(use-package jabber
  :load-path user-lisp-directory
  :bind (("C-x j" . jabber-connect))
  :load-path "~/lisp/im.el"
  :config
  (progn
    (setq-default jabber-account-list
		  #'((:password: nil)
		     (:network-server . "")
		     (:port 5220)
		     (:connection-type . ssl)))))

(use-package helm
  :bind (("C-c h" . helm-command-prefix)
	 ("<tab>" . helm-execute-persistent-action)
	 ("C-i" . helm-execute-persistent-action)
	 ("C-z" . helm-select-action))
  :preface
  (progn
    (use-package helm-config))
  (progn
    (global-unset-key (kbd "C-x c"))
    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t)))
  :config
  (progn
    ; open helm buffer inside current window, not occupy whole other window
    (setq helm-split-window-in-side-p t)
    ;; move to end or beginning of source when reaching top or bottom of source.
    (setq helm-move-to-line-cycle-in-source t)
    ;; search for library in `require' and `declare-function' sexp.
    (setq helm-ff-search-library-in-sexp t)
    ;; scroll 8 lines other window using M-<next>/M-<prior>
    (setq helm-scroll-amount 8)
    (setq helm-ff-file-name-history-use-recentf t)))

(use-package pyautomagic
  :load-path user-lisp-directory)

(use-package thememgr
  :load-path user-lisp-directory)

(setq debug-on-error t)
(setq magit-last-seen-setup-instructions "1.4.0")
(setq custom-theme-directory (locate-user-emacs-file "themes"))
(setq custom-theme-allow-multiple-selections nil)
(setq-default theme-load-from-file t)
(menu-bar-mode 0)
(helm-mode 1)

;; common lisp
(setq-default quicklisp-el "~/quicklisp/slime-helper.el")
(when (file-exists-p quicklisp-el)
  (load (expand-file-name quicklisp-el))
  (setq inferior-lisp-program "sbcl"))

(provide '.emacs-custom)
;;; .emacs-custom.el ends here
