;;====================================================================
;; Edd-mode
;;====================================================================
(autoload 'edd-mode "edd-mode"
  "Electronic Device Description Language" t)
(add-to-list
 'auto-mode-alist
 '("\\.dd$" . edd-mode))
