;;; im -- Integration for instant messaging
;;;
;;; Commentary:
;;;   Uses jabber mode.
;;; Customisations:
;;;  - Adapts python-mode to work with differnet project styles,
;;;    notably the Pylons project.
;;
;;; Code:

(require 'jabber)
(require 'notify)

(defun notify-jabber-notify (from buf text proposed-alert)
  "Notify via notify.el about new messages using FROM BUF TEXT PROPOSED-ALERT."
  (when (or jabber-message-alert-same-buffer
	    (not (memq (selected-window) (get-buffer-window-list buf))))
    (if (jabber-muc-sender-p from)
	(notify (format "(PM) %s"
			(jabber-jid-displayname (jabber-jid-user from)))
		(format "%s: %s" (jabber-jid-resource from) text)))
    (notify (format "%s" (jabber-jid-displayname from))
	    text)))

(add-hook 'jabber-alert-message-hooks #'notify-jabber-notify)

(provide 'im)
;;; im.el ends here
