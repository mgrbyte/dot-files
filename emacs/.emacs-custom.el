;;; Package -- Matt Russell's custom emacs setup -*- lexical-binding: t; coding: utf-8; mode: lisp -*-
;;;
;;; Commentary:
;;;    Integrates with mgrbyte-emacs.
;;; Code:

(require 'org)
(require 'use-package)

(defvar user-lisp-directory (expand-file-name "~/elisp")
  "Place to load local LISP code from.")

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

(use-package helm-projectile
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)
  (persp-mode))

(use-package package
  :bind (("C-c C-l" . list-packages)))

(use-package perspective)
(use-package persp-projectile)

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
  (add-to-list 'pyvenv-post-activate-hooks #'pyvenv-restart-python))

(use-package python
  :bind (("RET" . newline-and-indent))
  :init
  (add-hook #'python-mode-hook
	    (lambda ()
	      (require 'pyautomagic)
	      (pyvenv-mode 1)
	      (pyautomagic--venv-for-current-git-repo))))

(use-package jedi
  :config
  (jedi:ac-setup)
  (setq jedi:import-python-el-settings 't)
  (setq jedi:complete-on-dot 't)
  (bind-key "." #'jedi:goto-definition-pop-marker esc-map)
  (bind-key "S-." #'jedi:goto-definition-push-marker esc-map))

(use-package rst
  :init
  (auto-fill-mode t)
  (pyvenv-mode 1))

(use-package tex-mode
  :preface
  (defun turn-on-outline-minor-mode ()
    "Turn on the outline minor mode."
    (outline-minor-mode 1)
    (add-hook 'LaTeX-mode-hook 'turn-on-outline-minor-mode)
    (add-hook 'latex-mode-hook 'turn-on-outline-minor-mode)
    (setq outline-minor-mode-prefix "C-c C-o"))
  :config
  (setq-default
   LaTeX-eqnarray-label "eq"
   LaTeX-equation-label "eq"
   LaTeX-figure-label "fig"
   LaTeX-myChapter-label "chap"
   LaTeX-section-hook '(LaTeX-section-heading
			LaTeX-section-title
			LaTeX-section-toc
			LaTeX-section-section
			LaTeX-section-label)
   LaTeX-table-label "tab"
   TeX-auto-save t
   TeX-auto-save t
   TeX-newline-function #'reindent-then-newline-and-indent
   TeX-parse-self t
   TeX-parse-self t
   Tex-save-query nil)
  (autoload #'reftex-mode "reftex" "RefTeX Minor Mode" t)
  (autoload #'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
  (autoload #'reftex-citation "reftex-cite" "Make citation" nil)
  (autoload #'reftex-index-phrase-mode "reftex-index" "Phrase Mode" t)
  (add-hook #'latex-mode-hook #'turn-on-reftex)
  (add-hook #'LaTeX-mode-hook #'turn-on-reftex))

(use-package org
  :config
  (progn
    (defun mgrbyte--org-use-speed-commands-for-headings-and-lists ()
      "Activate speed commands on list items too."
      (or (and (looking-at org-outline-regexp) (looking-back "^\**"))
	  (save-excursion
	    (and (looking-at (org-item-re)) (looking-back "^[ \t]*")))))

    (defun mgrbyte--org-mode-ask-effort ()
      "Ask for an effort estimate when clocking in."
      (require 'org)
      (unless (org-entry-get (point) "Effort")
	(let ((effort
	       (completing-read
		"Effort: "
		(org-entry-get-multivalued-property (point) "Effort"))))
	  (unless (equal effort "")
	    (org-set-property "Effort" effort)))))

    (setq org-log-done 'time)
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
    (setq org-agenda-files
	  (f-entries "~/org" (apply-partially #'s-ends-with? ".org") t))
    (setq org-directory "~/org")
    (setq org-default-notes-file "~/org/refile.org")
    (setq org-use-effective-time t)
    (setq org-goto-interface 'outline org-goto-max-level 10)
    (setq org-startup-folded nil)
    (setq org-cycle-include-plain-lists 'integrate)
    (add-to-list 'org-speed-commands-user
		 '("x" org-todo "DONE"))
    (add-to-list 'org-speed-commands-user
		 '("y" org-todo-yesterday "DONE"))
    (add-to-list 'org-speed-commands-user
		 '("!" my/org-clock-in-and-track))
    (add-to-list 'org-speed-commands-user
		 '("s" call-interactively 'org-schedule))
    (add-to-list 'org-speed-commands-user
		 '("d" my/org-move-line-to-destination))
    (add-to-list 'org-speed-commands-user
		 '("i" call-interactively 'org-clock-in))
    (add-to-list 'org-speed-commands-user
		 '("o" call-interactively 'org-clock-out))
    (add-to-list 'org-speed-commands-user
		 '("$" call-interactively 'org-archive-subtree))
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
     'org-babel-load-languages
     '((emacs-lisp . t)
       (python . t)))
    (eval-after-load 'org-agenda
      '(bind-key "i" 'org-agenda-clock-in org-agenda-mode-map)))
  (add-hook 'org-clock-in-prepare-hook 'mgrbyte--org-mode-ask-effort))

(use-package mgrbyte
  :config
  (setq rst-slides-program "google-chrome")
  ;; Position frame geometry
  (setq cider-repl-history-file
	(f-join  (getenv "HOME") ".cider-repl-history"))
  (setq initial-frame-alist
	'((top . 0) (left . 420)
	  (width . 80) (height . 300))))

(provide '.emacs-custom)
;;; .emacs-custom.el ends here
