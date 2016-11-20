;;; Package -- Matt Russell's custom emacs setup -*- lexical-binding: t; coding: utf-8; mode: lisp -*-
;;;
;;; Commentary:
;;;    Integrates with mgrbyte-emacs.
;;; Code:
(require 'use-package)

(defvar user-lisp-directory (expand-file-name "~/elisp")
  "Place to load local LISP code from.")

(use-package gnus
  :bind (("C-c C-x m" . gnus)))

(use-package reload-dir-locals
  :load-path user-lisp-directory
  :bind (("C-c d l r" . reload-dir-locals-for-current-buffer)))

(use-package thememgr
  :load-path user-lisp-directory)

(use-package mgrbyte
  :init
  (toggle-frame-maximized))

(provide '.emacs-custom)
;;; .emacs-custom.el ends here
