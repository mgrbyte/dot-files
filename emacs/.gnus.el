;;; gnus --- Gnus config
;;;
;;; Commentary:
;;;     Matt R's gnus configuration
;;; Code:
(require 'epg-config)
(require 'f)
(require 'nnimap)
(require 'shr)
(require 'use-package)

(defvar authinfo-creds (expand-file-name "~/.authinfo.gpg"))

(defun mgrbyte/gnus-group-list-subscribed-groups ()
  "List all subscribed groups with or without un-read messages."
  (interactive)
  (require 'gnus)
  (gnus-group-list-all-groups 5))

(defun mgrbyte/gpg-fingerprint (email-address)
  "Get the GPG fingerprint for given EMAIL-ADDRESS."
  (shell-command-to-string
   (concat  "gpg2 --fingerprint"
	    " "
	    email-address
	    " | perl -n -e '/fingerprint =\s+(.*)$/ && print $1'")))

(defun mgrbyte/select-message-signature (signature-template)
  "Set the `message-signature-file' from a given SIGNATURE-TEMPLATE.

Select interactively from files present in `message-signature-directory'."
  (interactive "f")
  (message signature-template))

(use-package sendmail
  :config
  (setq-default send-mail-function #'smtpmail-send-it))


;; smtp
(use-package smtpmail
    :init
    (setq-default smtpmail-auth-credentials authinfo-creds
		  smtpmail-smtp-user (getenv "SMTPUSER")
		  smtpmail-smtp-user (getenv "SMTPSERVER")
		  smtpmail-debug-info t))

(use-package bbdb)

;; message
(use-package message
  :config
  (setq-default
   message-send-mail-function #'smtpmail-send-it
   message-signature-directory (f-expand "~Mail/signatures")
   message-signature #'mgrbyte/select-message-signature
   message-citation-line-function 'message-insert-formatted-citation-line
   message-citation-line-format "On %a, %b %d %Y at %r, %f wrote:")
  (add-hook 'message-mode-hook
	    '(lambda ()
	       (bbdb-initialize #'message)
	       (bbdb-initialize #'gnus)
	       (local-set-key "<TAB>" #'bbdb-complete-name))))


;; EasyPG
(use-package epa
  :config
  (setq-default epa-file-cache-passphrase-for-symmetric-encryption t
		epg-user-ud "43892AC8"
		epg-debug t))

(use-package mm-decode
  :config
  (add-to-list 'mm-attachment-override-types "image/.*")
  (add-to-list 'mm-inlined-types "text/html")
  (setq-default mm-text-html-renderer 'shr
		mm-verify-option 'always
		mm-decrypt-option 'always))

(use-package mm-encode
  :config
  (setq-default mm-sign-option 'guided
		mm-encrypt-option 'guided))


(use-package mml2015
  :config
  (setq-default
   mml2015-use 'epg
   mml2015-verbose t
   mml2015-encrypt-to-self t
   mml2015-always-trust nil
   mml2015-cache-passphrase t
   mml2015-passphrase-cache-expiry '36000
   mml2015-sign-with-sender t))

(use-package gnus
  :config
  (bind-key
   "d" #'gnus-summary-mark-as-expirable gnus-summary-mode-map)
  (bind-key
   "o" #'mgrbyte--gnus-group-list-subscribed-groups gnus-group-mode-map)
  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
  (setq-default
   gnus-permanently-visible-groups "INBOX"
   gnus-thread-hide-subtree t
   gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B%s%)\n"
   gnus-user-date-format-alist '((t . "%d-%m-%Y %H:%M"))
   gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
   gnus-thread-sort-functions '(gnus-thread-sort-by-date)
   gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"
   gnus-buttonized-mime-types '("multipart/alternative"
				"multipart/encrypted"
				"multipart/signed")
   gnus-message-replyencrypt t
   gnus-message-replysign t
   gnus-message-replysignencrypted t
   gnus-treat-x-pgp-sig t)
  (setq-default gnus-select-method
		'(nnimap "mgrbyte"
			 (nnimap-user "mgrbyte.co.uk")
			 (nnimap-address "imap.hosts.co.uk")
			 (nnimap-authinfo-file authinfo-creds)))
  (add-to-list 'gnus-secondary-select-methods
	       '(nnimap "horizon5"
			(nnimap-user "horizon5.org")
			(nnimap-address "imap.hosts.co.uk")
			(nnimap-authinfo-file authinfo-creds))))


;;; .gnus ends here
