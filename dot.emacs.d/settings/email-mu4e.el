;; mu4e configurations after loading mu4e
(with-eval-after-load 'mu4e
  (setq mail-user-agent 'mu4e-user-agent)
  (setq mu4e-view-mode-hook '(visual-line-mode fci-mode))
  (setq mu4e-view-use-gnus t)

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
	    ))

  (setq
   mu4e-compose-signature-auto-include nil;; no signature per default
   mu4e-change-filenames-when-moving t	;; needed for mbsync
   mu4e-update-interval 180		;; update mail every 3 min
   mu4e-decryption-policy t		;; decrypt all msgs (nil,ask) 
   message-kill-buffer-on-exit t	;; don't keep message buffers around
   mu4e-compose-dont-reply-to-self t	;;
   mu4e-view-prefer-html nil            ;; 
   mu4e-attachment-dir  (expand-file-name "~/Downloads/")	;; attachments go here
   )

  (setq mu4e-view-html-plaintext-ratio-heuristic most-positive-fixnum)
  
  (add-hook 'mu4e-view-mode-hook 'visual-line-mode)
  (setq mu4e-html2text-command "w3m -dump -T text/html")
  ;; the headers to show in the headers list -- a pair of a field
  ;; and its width, with `nil' meaning 'unlimited'
  ;; (better only use that for the last field.
  ;; These are the defaults:
  (setq mu4e-headers-fields
	'( (:date          .  25)    ;; alternatively, use :human-date
	   (:flags         .   6)
	   (:from          .  22)
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

