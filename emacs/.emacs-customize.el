(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
    '(ansi-color-faces-vector
         [default bold shadow italic underline bold bold-italic bold])
    '(ansi-color-names-vector
         (vector "#c5c8c6" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#8abeb7" "#1d1f21"))
 '(custom-enabled-themes (quote (abyss)))
    '(custom-safe-themes
         (quote
             ("fea8cdb03f7aca2eeeb476cb5b679b8b63a23438d9ecbef1326576c7495c06c9" "0820d191ae80dcadc1802b3499f84c07a09803f2cb90b343678bdb03d225b26b" default)))
    '(safe-local-variable-values
         (quote
             ((eval when
                  (and
                      (buffer-file-name)
                      (file-regular-p
                          (buffer-file-name))
                      (string-match-p "^[^.]"
                          (buffer-file-name)))
                  (emacs-lisp-mode)
                  (when
                      (fboundp
                          (quote flycheck-mode))
                      (flycheck-mode -1))
                  (unless
                      (featurep
                          (quote package-build))
                      (let
                          ((load-path
                               (cons ".." load-path)))
                          (require
                              (quote package-build))))
                  (package-build-minor-mode)
                  (set
                      (make-local-variable
                          (quote package-build-working-dir))
                      (expand-file-name "../working/"))
                  (set
                      (make-local-variable
                          (quote package-build-archive-dir))
                      (expand-file-name "../packages/"))
                  (set
                      (make-local-variable
                          (quote package-build-recipes-dir))
                      default-directory))
                 (dired-omit-files)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(jabber-activity-personal-face ((t (:foreground "gold" :weight bold))))
 '(jabber-chat-prompt-foreign ((t (:foreground "magenta" :weight bold))))
 '(jabber-chat-prompt-local ((t (:foreground "white smoke" :weight bold))))
 '(jabber-rare-time-face ((t (:foreground "gray" :underline t))))
 '(jabber-roster-user-online ((t (:foreground "cornsilk" :slant normal :weight bold)))))
