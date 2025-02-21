;; mu4e configurations after loading mu4e
(with-eval-after-load 'mu4e
  ;; the next are relative to the root maildir
  ;; (see `mu info`).
  ;; instead of strings, they can be functions too, see
  ;; their docstring or the chapter 'Dynamic folders'
  (setq mu4e-sent-folder   "/ruschival.de/Sent"
	mu4e-drafts-folder "/ruschival.de/Drafts"
	mu4e-trash-folder  "/ruschival.de/Trash")

  ;; the maildirs you use frequently; access them with 'j' ('jump')
  (setq   mu4e-maildir-shortcuts
	  '((:maildir "/ruschival.de/INBOX"   :key ?i)
	    (:maildir "/ruschival.de/Sent"    :key ?s)
	    (:maildir "/ruschival.de/Drafts"  :key ?d)
	    (:maildir "/ruschival.de/Shopping"   :key ?h)
	    (:maildir "/ruschival.de/Trash"   :key ?t)
	    ))

  ;; (add-to-list 'mu4e-bookmarks
  ;; '( :name  "emails 3days - no Mailinglist"
  ;;    :query "NOT maildir:/ruschival.de/MailingLists/* AND date:3d.."
  ;;    :key   ?X))
  (setq mu4e-bookmarks
	'(
	  (:name "last 7 days (no list)"
		 :query "NOT v:buildroot.buildroot.org AND date:15d.."
		 :key ?n)
	  (:name "unread messages (no list)"
		 :query "flag:unread AND NOT v:buildroot.buildroot.org "
		 :key ?u)
	  (:name "mailinglists (buildroot)"
		 :query "v:buildroot.buildroot.org"
		 :key ?l)
	  (:name "Flag Trashed"
		 :query "flag:trashed "
		 :key ?t)
	  ))

  ;; Default encryption and sign keys
  (setq
   mm-sign-option nil
   mml-secure-key-preferences
   '((OpenPGP
      (sign
       ("thomas@ruschival.de" "67EFDAAE56716D78D8D1A307801A13F19F7ACBB9"))
      (encrypt
       ("thomas@ruschival.de" "67EFDAAE56716D78D8D1A307801A13F19F7ACBB9")))
     (CMS
      (sign)
      (encrypt))))

  (setq
   mail-user-agent 'mu4e-user-agent
   mu4e-compose-signature-auto-include nil;; no signature per default
   mu4e-change-filenames-when-moving t	;; needed for mbsync
   mu4e-update-interval 600		;; update mail every 10 min
   mu4e-decryption-policy t		;; decrypt all msgs (nil,ask)
   message-kill-buffer-on-exit t	;; don't keep message buffers around
   mu4e-compose-in-new-frame nil	;; Open new frame for writing emails
   mu4e-compose-format-flowed nil	;; Soft Line-Breaks depending on client
   mu4e-compose-dont-reply-to-self t	;;
   mu4e-attachment-dir  (expand-file-name "~/Downloads/")	;; attachments go here
   message-citation-line-function 'message-insert-formatted-citation-line
   message-citation-line-format "On %Y-%m-%d at %R %Z, %f wrote:\n"
   )

  ;; Use gnus as mail reader, capable of decoding inline PGP
  ;; (setq mu4e-view-use-gnus t)
  ;; (setq gnus-blocked-images "http")
  ;; Some magic to show html mail
  ;; (setq mu4e-view-prefer-html nil)
  ;; (setq mu4e-html2text-command "html2text -utf8 -width 72")
  ;; (setq mu4e-html2text-command "python3 -m html2text --no-wrap-links")
  ;; (setq mu4e-view-html-plaintext-ratio-heuristic 10000)
  (setq mm-discouraged-alternatives '("text/html" "image/.*"))
  (setq shr-color-visible-luminance-min 40)
  (setq shr-use-colors t)
  (setq mu4e-headers-report-render-time t)

  (add-hook 'mu4e-view-mode-hook(
	     lambda ()
	     (message "mu4e-view-mode-hook" )
	     ;; no trailing whitespace highlight for messages
	     (setq show-trailing-whitespace nil)
	     ;;(turn-off-fci-mode)
	     ;; break lines at end of window
	     (setq visual-line-mode t)
	     ;; No line number side bar in header view or message view
	     (display-line-numbers-mode -1)
	     ;; local key binding for view actions
	     (local-set-key (kbd "v") 'mu4e-view-action)
	     )
	    )
  ;; No line number side bar in header view or message view
  (add-hook 'mu4e-headers-mode-hook (lambda ()
				      (display-line-numbers-mode -1)
				      ;; local key binding for view actions
			      	      (local-set-key (kbd "v") 'mu4e-view-action)
				      ))

  (setq mu4e-compose-crypto-reply-plain-policy 'sign)
  ;; (add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime)
  (setq mml-secure-openpgp-encrypt-to-self t)
  (setq mml-secure-openpgp-sign-with-sender t)

  ;; the headers to show in the headers list -- a pair of a field
  ;; and its width, with `nil' meaning 'unlimited'
  ;; (better only use that for the last field.
  ;; These are the defaults:
  (setq mu4e-headers-fields
	'( (:human-date    .  20)    ;; alternatively, use :human-date
	   (:flags         .   6)
	   (:size          .   6)
	   (:from-or-to    .  22)
	   (:subject       .  nil))) ;; alternatively, use :thread-subject
   (setq mu4e-headers-date-format "%+4Y-%m-%d")

  ;; program to get mail; alternatives are 'fetchmail', 'getmail'
  ;; isync or your own shellscript. called when 'U' is pressed in
  ;; main view.
  ;; If you get your mail without an explicit command,
  ;; use "true" for the command (this is the default)
  (setq mu4e-get-mail-command "mbsync -a")
  ;; (setq mu4e-get-mail-command "true")

  ;; general emacs mail settings; used when composing e-mail
  ;; the non-mu4e-* stuff is inherited from emacs/message-mode
  (setq mu4e-compose-reply-to-address "thomas@ruschival.de"
	user-mail-address "thomas@ruschival.de"
	user-full-name  "Thomas Ruschival")

  (defun ok/message-attachment-present-p ()
    "Return t if a non-gpg attachment is found in the current message."
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(when (search-forward "<#part type" nil t) t))))

  (setq ok/message-attachment-regexp
	(regexp-opt '("[Ww]e send"
		      "[Ii] send"
		      "attach"
		      "[aA]ngehängt"
		      "[aA]nhang"
		      "[sS]chicke"
		      "angehaengt"
		      "haenge"
		      "hänge")))

  (defun ok/message-warn-if-no-attachments ()
    "Check if there is an attachment in the message if I claim it."
    (when (and (save-excursion
		 (save-restriction
		   (widen)
		   (goto-char (point-min))
		   (re-search-forward ok/message-attachment-regexp nil t)))
	       (not (ok/message-attachment-present-p)))
      (unless (y-or-n-p "No attachment. Send the message?")
	(keyboard-quit))))

  (add-hook 'message-send-hook #'ok/message-warn-if-no-attachments)

  ;; smtp mail setting
  (setq
   message-send-mail-function 'smtpmail-send-it
   starttls-use-gnutls t
   smtpmail-smtp-server "mail.rolf-dv.de"
   smtpmail-smtp-service 587
   ;; if you need offline mode, set these -- and create the queue dir
   ;; with 'mu mkdir', i.e.. mu mkdir /home/user/Maildir/queue
   smtpmail-queue-mail  nil
   smtpmail-queue-dir  (expand-file-name "~/Maildir/ruschival.de/Queue")
   smtpmail-debug-info t
   smtpmail-debug-verb t
   )

  ;; Maildirs
  ;; (require 'mu4e-maildirs-extension)
  ;; (mu4e-maildirs-extension)

  ;; Calendar
  (require 'mu4e-icalendar)
  (mu4e-icalendar-setup)
  (setq mu4e-icalendar-diary-file "~/Nextcloud/calendar/diary.org")
  (setq mu4e-icalendar-trash-after-reply t)


  (require 'org-agenda)
  (setq gnus-icalendar-org-capture-file "~/Nextcloud/calendar/calendar.org")
  (setq org-agenda-files '("~/Nextcloud/calendar/"))
  (setq gnus-icalendar-org-capture-headline '("Calendar"))
  (gnus-icalendar-org-setup)

  )
(provide 'email-mu4e)

(concat (getenv "HOME") )
