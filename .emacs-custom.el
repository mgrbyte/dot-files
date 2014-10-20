;;; .emacs-custom.el --- This is your custom configuration file (`custom-file')
;; This is your default customistions file.
;; The emacs customize interace will append to this file.

;;; Commentary:
;;

;;; Code:

(require 'ido)
(require 'use-package)
(require 'org-install)
(require 'epresent)
(use-package ace-jump-mode)
(use-package ws-butler)

(setq-default dired-omit-files-p t)
(setq custom-theme-directory (locate-user-emacs-file "themes"))

(bind-key "C-c SPC" 'ace-jump-mode)

(ido-mode 1)
(setq ido-case-fold t)
(setq ido-everywhere t)
(setq ido-enable-prefix nil)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(setq ido-max-prospects 10)
(setq ido-file-extensions-order
      '(".py" ".zcml" ".el" ".xml" ".js"))

;; Load expermients?
;; (load-experiment "python-auto-magic.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (warm-night)))
 '(custom-safe-themes (quote ("7b0433e99dad500efbdf57cf74553499cde4faf2908a2852850c04b216c41cc9" "146d24de1bb61ddfa64062c29b5ff57065552a7c4019bee5d869e938782dfc2a" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "11d069fbfb0510e2b32a5787e26b762898c7e480364cbc0779fe841662e4cf5d" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "55e0b2d72ea7eec555e36242e4b5008d809504767d1f261d78c28b1245da95a7" "9f403454579affa3abd21364e97d5224a318df342f8fe8af95138996d478f8cb" "2b584cee821f2646c4977eb8061a2a75a4e6c57d211220cf14f4e92960393e4e" default)))
 '(fci-rule-color "#eee8d5")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors (--map (solarized-color-blend it "#fdf6e3" 0.25) (quote ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors (quote (("#eee8d5" . 0) ("#B4C342" . 20) ("#69CABF" . 30) ("#69B7F0" . 50) ("#DEB542" . 60) ("#F2804F" . 70) ("#F771AC" . 85) ("#eee8d5" . 100))))
 '(jedi:complete-on-dot t)
 '(jedi:server-args nil)
 '(magit-diff-use-overlays nil)
 '(magit-use-overlays nil)
 '(send-mail-function (quote smtpmail-send-it))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(smtpmail-smtp-server "mail.netsight.co.uk")
 '(smtpmail-smtp-service 25)
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map (quote ((20 . "#dc322f") (40 . "#c85d17") (60 . "#be730b") (80 . "#b58900") (100 . "#a58e00") (120 . "#9d9100") (140 . "#959300") (160 . "#8d9600") (180 . "#859900") (200 . "#669b32") (220 . "#579d4c") (240 . "#489e65") (260 . "#399f7e") (280 . "#2aa198") (300 . "#2898af") (320 . "#2793ba") (340 . "#268fc6") (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(vc-command-messages t)
 '(weechat-color-list (quote (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq theme-load-from-file t)
(setq theme-default 'solarized-dark)
(menu-bar-mode 1)
(global-netsight-mode 1)

;; (when (window-system)
;;   (set-frame-font "Source Code Pro")
;;   (set-face-attribute 'default nil :font "Source Code Pro" :height 110)
;;   (set-face-font 'default "Source Code Pro"))

;; Re-define netsight-keys for hhkb
(defun hhkb-remap-keys ()
  "Re-map keys when using hhkb keyboard."
  (let ((hhkb-not-in-use "HHKB-NOT-IN-USE"))
    (unless (eq (access-file "~/.hhkb" hhkb-not-in-use) hhkb-not-in-use)
      (define-key netsight-keymap (kbd "<f6>") 'magit-status)
      (define-key netsight-keymap (kbd "<f3>") 'split-window-horizontally)
      (define-key netsight-keymap (kbd "<f5>") 'netsight-insert-debug))))

(hhkb-remap-keys)

(global-set-key (kbd "C-c +") 'text-scale-increase)
(global-set-key (kbd "C-c -") 'text-scale-decrease)

(add-hook 'dired-load-hook
    '(lambda ()
         (require 'dired-x)
         (dired-omit-mode 1)))

(add-hook 'nxml-mode-hook
    '(lambda ()
         (setq tab-width 2)
         (setq indent-tabs-mode t)))


(defun org-turn-on-iimage-in-org ()
    "Display images in your org file."
    (interactive)
    (turn-on-iimage-mode)
    (set-face-underline-p 'org-link nil))

(defun org-toggle-iimage-in-org ()
    "Display images in your org file."
    (interactive)
    (if (face-underline-p 'org-link)
        (set-face-underline-p 'org-link nil)
        (set-face-underline-p 'org-link t))
    (call-interactively 'iimage-mode))

(add-hook 'python-mode-hook 'remap-comment-dwim)
(add-hook 'conf-mode-hook 'remap-comment-dwim)
(add-hook 'org-mode-hook '(lambda () (org-turn-on-iimage-in-org)))

(load-theme 'solarized-dark)
(provide '.emacs-custom)

;;; .emacs-custom.el ends here
