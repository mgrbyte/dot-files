;;; gnus --- Gnus config
;;;
;;; Commentary:
;;;     Matt R's gnus configuration
;;; Code:
(require 'epg-config)
(require 'nnimap)
(require 'nnir)
(require 'shr)

(defvar authinfo-creds "~/.gnupg/authinfo.gpg")
(defvar netsight-mail-server "mail.netsight.co.uk")
(defvar netsight-user-name "mattr")
(defvar netsight-smtp-port "465")

(define-key gnus-summary-mode-map "d" #'gnus-summary-mark-as-expirable)

(setq-default user-full-name "Matt Russell")
(setq-default epa-file-cache-passphrase-for-symmetric-encryption t)
(setq-default smtpmail-auth-credentials authinfo-creds)
(setq gnus-select-method '(nnimap "mail.netsight.co.uk"
				  (nnimap-inbox "INBOX")
				  (nnimap-adddress "mail.netsight.co.uk")
				  (nnir-search-engine imap)
				  (nnimap-authinfo-file authinfo-creds)
				  (nnmail-expiry-wait 90)))
(setq gnus-permanently-visible-groups "INBOX")
(setq-default
 gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B%s%)\n"
 gnus-user-date-format-alist '((t . "%d-%m-%Y %H:%M"))
 gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
 gnus-thread-sort-functions '(gnus-thread-sort-by-date))
(setq message-citation-line-function 'message-insert-formatted-citation-line)
(setq message-citation-line-format "On %a, %b %d %Y at %r, %f wrote:")
(setq gnus-thread-hide-subtree t)
(setq-default user-mail-address (format "%s@netsight.co.uk" netsight-user-name))
(setq-default send-mail-function #'smtpmail-send-it)
(setq-default message-send-mail-function #'smtpmail-send-it)
(setq-default smtpmail-default-smtp-server netsight-mail-server)
(setq-default smtpmail-auth-credentials
	      '((netsight-mail-server
		 netsight-smtp-port
		 netsight-user-name
		 nil)))
(setq-default smtpmail-starttls-credentials
	      '((netsight-mail-server netsight-smtp-port nil nil)))
;; MM
(setq mm-text-html-renderer 'shr)
(add-to-list 'mm-attachment-override-types "image/.*")
(add-to-list 'mm-inlined-types "text/html")

;; EasyPG
(setq-default mml2015-use 'epg
	      mml2015-verbose t
	      epg-user-id "B97B84F2"
	      mml2015-encrypt-to-self t
	      mml2015-always-trust nil
	      mml2015-cache-passphrase t
	      mml2015-passphrase-cache-expiry '36000
	      mml2015-sign-with-sender t
	      gnus-message-replyencrypt t
	      gnus-message-replysign t
	      gnus-message-replysignencrypted t
	      gnus-treat-x-pgp-sig t
              mm-sign-option 'guided
              mm-encrypt-option 'guided
	      mm-verify-option 'always
	      mm-decrypt-option 'always
	      gnus-buttonized-mime-types
	      '("multipart/alternative"
		"multipart/encrypted"
		"multipart/signed")
	      ;;  then read the *epg-debug*" buffer
	      epg-debug t)
;;; .gnus ends here
