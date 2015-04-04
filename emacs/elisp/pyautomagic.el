;;; package -- Matt Russell's custom Emacs/Python integration.
;;;
;;; Commentary:
;;; Integrates with netsight-emacs.
;;; Customisations:
;;;  - Adapts python-mode to work with differnet project styles,
;;;    notably the Pylons project.
;;
;;; Code:

(defvar pylons-git-repos
  (list
    "colander"
    "deform"
    "peppercorn"
    "pyramid"
    "pyramid_chameleon"
    "pyramid_mako"
    "pyramid_layout"
    "pyramid_ldap"
    "pyramid_tm"
    "pyramid_zcml"
    "pyramid_zodbconn"
    "sdidemo"
    "substanced"
    "webob"
    "waitress"))

(defun pyautomagic--get-git-repo-name (url)
  "Return the git repository name given a URL.

A git URL ends wtih the suffix `.git`.
Return nil if this is not the case."
  (let* ((suffix ".git")
         (url-parts (s-split "/" url))
         (repo-name (car (last url-parts))))
    (if (s-suffix? suffix repo-name)
      (car (s-split suffix repo-name)))))

(defun pyautomagic--is-pylons-project-repository (url)
  "Return true if URL is a Pylons repository."
  (member (pyautomagic--get-git-repo-name url) pylons-git-repos))

(defun pyautomagic--git-get-current-remote-name ()
  "Get the current git remote name if any."
  (let* ((branch  (magit-get-current-branch))
         (remote (magit-get "branch" branch "remote")))
    (when remote
      (magit-get "remote" remote "url"))))

(defun pyautomagic--flake8-for-current-git-repo()
  "Set the flake8rc file for the current git repository."
  (let* ((curr-git-remote-url (pyautomagic--git-get-current-remote-name))
	 (flake8rc-filename "flake8rc"))
    (if (and curr-git-remote-url
	     (pyautomagic--is-pylons-project-repository curr-git-remote-url))
	(setq flake8rc-filename "pylons.flake8rc"))
    (setq-default flycheck-flake8rc (concat "~/.config/" flake8rc-filename))))

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

(defun pyautomagic--venv-for-current-git-repo ()
  "Perhaps invoke `pyvenv-workon' dependant on a possible git repository."
  (let* ((git-remote-name (pyautomagic--git-get-current-remote-name))
	 (git-repo-name (or (file-name-base git-remote-name) ""))
	 (venv-names (pyautomagic--venv-known-names (pyvenv-workon-home)))
	 (repo-venv (apply-partially #'s-contains? git-repo-name))
	 (venvs-matched (-filter repo-venv venv-names)))
    (if (and (> 0 (length venvs-matched)) pyvenv-virtual-env)
	(pyvenv-deactivate)
      (pyvenv-workon (car venvs-matched)))))

(use-package jedi
  :config
  (progn
    (setq jedi:complete-on-dot 't)))

(use-package pyvenv
  :bind (("C-c w" . pyvenv-workon)
	 ("C-c v d" . pyvenv-deactivate)
	 ("C-c v e" . pyautomagic--activate-venv-safely)))

(use-package python
  :bind (("RET" . newline-and-indent))
  :init
  (progn
    (add-hook #'python-mode-hook
	      (lambda ()
		(pyvenv-mode 1)
		(pyautomagic--flake8-for-current-git-repo)
		(pyautomagic--venv-for-current-git-repo)))))

(use-package rst
  :init
  (progn
    (auto-fill-mode t)
    (pyvenv-mode 1)))

(provide 'pyautomagic)
;;; pyautomagic.el ends here
