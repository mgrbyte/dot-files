;;; package -- Matt Russell's custom Emacs/Python integration.
;;;
;;; Commentary:
;;; Integrates with netsight-emacs.
;;; Customisations:
;;;  - Adapts python-mode to work with differnet project styles,
;;;    notably the Pylons project.
;;
;;; Code:
(require 'dash)
(require 'f)
(require 'flycheck)
(require 'magit)
(require 'pyvenv)
(require 's)

(defvar pyautomagic--default-flycheck-checker
  "No Flake8 (Choose when editing 3rd party code)"
  "The default `flycheck' checker option.")

(defun pyautomagic--get-git-repo-name (url)
  "Return the git repository name given a URL.

A git URL ends wtih the suffix `.git`.
Return nil if this is not the case."
  (let* ((suffix ".git")
         (url-parts (s-split "/" url))
         (repo-name (car (last url-parts))))
    (if (s-suffix? suffix repo-name)
      (car (s-split suffix repo-name)))))

(defun pyautomagic--git-get-current-remote-name ()
  "Get the current git remote name if any."
  (let* ((branch  (magit-get-current-branch))
         (remote (or (magit-get "branch" branch "remote")
		     (magit-get "branch" "master" "remote"))))
    (when remote
      (magit-get "remote" remote "url"))))

(defun pyautomagic--is-pylons-project-repository (url)
  "Return true if URL is a Pylons repository."
  (s-contains? "Pylons/" url))


(defun pyautomagic--activate-venv-safely (directory)
  "Use instead of pyvenv-activate to strip trailing slash from DIRECTORY."
  (interactive "DEnter Path to directory containing bin/activate:")
  (pyvenv-activate (directory-file-name directory)))

(defun pyautomagic--venv-known-names (directory)
  "List `known` virtualenvs names only in DIRECTORY."
  (let* ((dir-name (directory-file-name directory))
	 (files (directory-files directory 1))
	 (starts-with-dot (apply-partially #'s-starts-with? "."))
	 (dir-names (-filter #'file-directory-p files))
	 (entries (-map #'file-name-nondirectory dir-names)))
    (-drop-while starts-with-dot entries)))

(defun pyautomagic--flake8rc-candidate-list ()
  "List possible flake8rc config files."
  (let* ((project-files (-filter #'f-file? (f-entries (magit-toplevel))))
	 (config-entries
	  (-filter (apply-partially #'s-ends-with? "flake8rc")
		   (f-entries "~/.config")))
	 (project-entries
	  (-filter (lambda (entry) (or (f-ext? entry "ini") (f-ext? entry "cfg")))
		   project-files)))
    (append config-entries project-entries)))

(defun pyautomagic--flycheck-checker-configured? (directory-name variable)
  "Does a dir-local in DIRECTORY-NAME contain a `flycheck' VARIABLE?.

Determined by looking at dir-locals for the current git
repository, and seeing if there is a `python-mode' entry with a
sub-entry for `flycheck-flake8rc' defined."
  (when directory-name
    (let* ((dlf (f-join (file-name-as-directory directory-name) dir-locals-file))
	   (class (if (f-exists? dlf) (dir-locals-read-from-file dlf) ""))
	   (variables (dir-locals-get-class-variables class))
	   (variables
	    (dir-locals-collect-variables
	     (dir-locals-get-class-variables class)
	     directory-name variables)))
      (when variables
	(dolist (elt variables)
	  (when (memq (car elt) '(python-mode))
	    (eq variable (car (car (cdr elt))))))))))

;;;###autoload
(defun pyautomagic--remember-flycheck-checker (&optional checker)
  "Remember the last selected CHECKER."
  (interactive)
  (let* ((curr-buffer (current-buffer))
	 (dl-buffer-name (f-join (magit-toplevel) dir-locals-file)))
    (unless (pyautomagic--flycheck-checker-configured?
	     (file-name-directory dl-buffer-name)
	     #'flycheck-checker)
      (find-file-literally (magit-toplevel))
      (add-dir-local-variable
       #'python-mode
       #'flycheck-checker
       (or checker flycheck-checker))
      (save-buffer)
      (kill-buffer)
      (find-file (buffer-file-name curr-buffer)))))

;;;###autoload
(defun pyautomagic--configure-flycheck-checkers (&optional path)
  "Configure `flycheck' checkers for a project.

If PATH is provided, configure a 'dir-locals.el' file in that
directory that uses a flake8rc files pointed to by PATH.

If there already exists a `dir-locals' file, check to see if a
`flycheck-checker' has been configured for `python-mode',
and if so do nothing.

Otherwise, prompt the user for the PATH to flake8rc config file,
using `pyautomagic--flake8rc-candidate-list'."
  (interactive)
  (let ((dld (car (dir-locals-find-file (magit-toplevel)))))
    (unless (pyautomagic--flycheck-checker-configured?
	     dld
	     #'flycheck-flake8rc)
      (setq path
	    (car
	     (list
	      (completing-read
	       "Flake8 config from: "
	       (pyautomagic--flake8rc-candidate-list)
	       nil t nil nil pyautomagic--default-flycheck-checker nil)))))
    (when (not (or (equal path "")
		   ;; Some completion frameworks can return nil for the
		   ;; default, see
		   ;; https://github.com/jorgenschaefer/elpy/issues/144
		   (equal path nil)))
      (save-excursion
        (find-file-literally (magit-toplevel))
        (if (equal path pyautomagic--default-flycheck-checker)
            (pyautomagic--remember-flycheck-checker #'python-pyflakes)
          (progn
            (add-dir-local-variable
             #'python-mode #'flycheck-flake8rc path)
            (pyautomagic--remember-flycheck-checker #'python-flake8)))))))

(defvar pyvenv-virtual-env)

(defun pyautomagic--venv-for-current-git-repo ()
  "Perhaps invoke `pyvenv-workon' dependant on a possible git repository."
  (let* ((git-remote-name (pyautomagic--git-get-current-remote-name))
	 (git-repo-name (s-downcase (or (file-name-base git-remote-name) "")))
	 (venv-names (pyautomagic--venv-known-names (pyvenv-workon-home)))
	 (repo-venv (apply-partially #'s-contains? git-repo-name))
	 (venvs-matched (-filter repo-venv venv-names)))
    (if (and (> 0 (length venvs-matched)) pyvenv-virtual-env)
        (pyvenv-deactivate)
      (pyvenv-workon (car venvs-matched)))))

(provide 'pyautomagic)
;;; pyautomagic.el ends here
