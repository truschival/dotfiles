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
	    (:maildir "/ruschival.de/Trash"   :key ?t)
	    ))

  ;; (add-to-list 'mu4e-bookmarks
  ;; '( :name  "emails 3days - no Mailinglist"
  ;;    :query "NOT maildir:/ruschival.de/MailingLists/* AND date:3d.."
  ;;    :key   ?X))
  (setq mu4e-bookmarks
	'(
	  (:name "emails 3 days (no list)"
		 :query "NOT flag:list AND date:3d.."
		 :key ?n)
	  (:name "unread messages (no list)"
		 :query "flag:unread AND NOT flag:list AND NOT flag:trashed "
		 :key ?u)
	  (:name "mailinglists (buildroot)"
		 :query "to:buildroot@busybox.net OR  to:buildroot@uclibc.org"
		 :key ?l)
	  ))

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
   mu4e-view-prefer-html nil            ;;
   mu4e-attachment-dir  (expand-file-name "~/Downloads/")	;; attachments go here
   message-citation-line-function 'message-insert-formatted-citation-line
   message-citation-line-format "On %Y-%m-%d at %R %Z, %f wrote:\n"
   )

  ;; Use gnus as mail reader, capable of decoding inline PGP
  ;; (setq mu4e-view-use-gnus t)

  ;; Some magic to show html mail
  ;; (setq mu4e-html2text-command "w3m -dump -T text/html")
  (setq mu4e-view-html-plaintext-ratio-heuristic most-positive-fixnum)

  (add-hook 'mu4e-view-mode-hook
	    (
	     lambda ()
	     (message "mu4e-view-mode-hook" )
	     ;; no trailing whitespace highlight for messages
	     (setq show-trailing-whitespace nil)
	     (turn-off-fci-mode)
	     ;; break lines at end of window
	     (setq visual-line-mode t)
	     ;; No line number side bar in header view or message view
	     (linum-mode -1)
	     )
	    )
  ;; No line number side bar in header view or message view
  (add-hook 'mu4e-headers-mode-hook (lambda ()
				      (linum-mode -1)))

  ;; Sign message before sending
  (setq mm-sign-option 'guided)
  (add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime)

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

  ;; smtp mail setting
  (setq
   message-send-mail-function 'smtpmail-send-it
   starttls-use-gnutls t
   smtpmail-starttls-credentials '(("mail.rolf-dv.de" 587 nil nil))
   smtpmail-auth-credentials
   '(("mail.rolf-dv.de" 587 "thomas@ruschival.de" nil))
   smtpmail-default-smtp-server "mail.rolf-dv.de"
   smtpmail-smtp-server "mail.rolf-dv.de"
   smtpmail-smtp-service 587

   ;; if you need offline mode, set these -- and create the queue dir
   ;; with 'mu mkdir', i.e.. mu mkdir /home/user/Maildir/queue
   smtpmail-queue-mail  nil
   smtpmail-queue-dir  (expand-file-name "/home/ruschi/Maildir/ruschival.de/Queue")
   )

  ;; Maildirs
  ;; (require 'mu4e-maildirs-extension)
  ;; (mu4e-maildirs-extension)
  )
(provide 'email-mu4e)
