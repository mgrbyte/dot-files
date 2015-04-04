;;; thememgr --- Manage Emacs themes
;;;
;;; Commentary:
;;;    Manages thems with `powerline` integration.
;;;
(use-package powerline)

;;;###autoload
(defun load-my-theme ()
  "Load my theme with powerline enabled."
  (interactive)
  (powerline-revert)
  (powerline-reset)
  (call-interactively #'load-theme)
  (powerline-default-theme))

(provide 'thememgr)
;;; thememgr.el ends here
