;;; Package -- reload dir locals
;;;
;;; Commentary:
;;;   Reload directory local variables.
;;;   Taken from:
;;;     http://emacs.stackexchange.com/questions/13080/reloading-directory-local-variables
;;; Code:

;;;###autoload
(defun reload-dir-locals-for-current-buffer ()
  "Reload dir locals for the current buffer."
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

;;;###autoload
(defun reload-dir-locals-for-all-buffer-in-default-directory ()
  "For every buffer with the same `default-directory` as the current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
	(when (equal default-directory dir))
	(reload-dir-locals-for-current-buffer)))))

(defun enable-autoreload-for-dir-locals ()
  "Enables auto-reloading of directory local variables."
  (when (and (buffer-file-name)
	     (equal dir-locals-file
		    (file-name-nondirectory (buffer-file-name))))
    (add-hook (make-variable-buffer-local 'after-save-hook)
	      #'reload-dir-locals-for-all-buffer-in-default-directory)))

(provide 'reload-dir-locals)
;;; reload-dir-locals.el ends here
