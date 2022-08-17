;;==============================================================================
;; Highlight symbol
;;==============================================================================
(global-set-key [f3] 'highlight-symbol-at-point)
(global-set-key [(meta f3)] 'unhighlight-regexp)

;;==============================================================================
;; Hotkeys
;;==============================================================================
(global-set-key "\C-x\C-d" 'dired) ;; override list-directory
(global-set-key "\C-c\C-e" 'mu4e)   ;; load email view
(global-set-key [f1] 'shell-command-on-region)
(global-set-key [f4] 'query-replace-regexp)
(global-set-key [f5] 'revert-buffer)
(global-set-key [f6] 'indent-region)
(global-set-key [f7] 'comment-region)
(global-set-key [f8] 'uncomment-region)
(global-set-key [f9] 'delete-trailing-whitespace)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-v" 'nil) ;; I DON't want C-v (Windows paste) mess here
(global-set-key "\M-v" 'nil) ;; I don't want M-v shouldn't do anything either
(global-set-key "\C-l" 'org-store-link)
(global-set-key "\C-H" 'delete-backward-char) ;; also delete with shift-held
(global-set-key "\C-x?" 'help-command)

(provide 'key-mappings)
