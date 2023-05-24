;;====================================================================
;; Map file-names to modes
;;====================================================================

;;====================================================================
;; Edd-mode
;;====================================================================
;; (autoload 'edd-mode "edd-mode"
;;   "Electronic Device Description Language" t)
;; (add-to-list 'auto-mode-alist '("\\.dd$" . edd-mode))
;; (add-to-list 'auto-mode-alist '("\\.ddl$" . edd-mode))
;; (add-to-list 'auto-mode-alist '("\\.edd$" . edd-mode))

;;==============================================================================
;; VHDL mode
;;==============================================================================
;; (autoload 'vhdl-mode "vhdl-mode" "VHDL Mode" t)
;; (add-to-list 'auto-mode-alist '("\\.vhd$" . vhdl-mode))
;; (add-to-list 'auto-mode-alist '("\\.vhdl$" . vhdl-mode))

;;==============================================================================
;; CMake Mode
;;==============================================================================
(autoload 'cmake-mode "cmake-mode" t)
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt$" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake$" . cmake-mode))

;;==============================================================================
;; QML
;;==============================================================================
(autoload 'qml-mode "qml-mode" "Editing Qt Declarative." t)
(add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))

;;==============================================================================
;; Make emacs interpret zsh files as shell 
;;==============================================================================
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

;;==============================================================================
;; Snippets
;;==============================================================================
;; (add-to-list 'auto-mode-alist '("yasnippet/snippets" . snippet-mode))
;; (add-to-list 'auto-mode-alist '("\\.yasnippet$" . snippet-mode))

;;==============================================================================
;; MATLAB
;;==============================================================================
(autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
(add-to-list  'auto-mode-alist  '("\\.m$" . matlab-mode))
(setq matlab-indent-function t)
(setq matlab-shell-command "matlab")
(setq matlab-shell-command-switches '("-nodesktop -nosplash"))


;;==============================================================================
;; JSON Mode
;;==============================================================================
;; (autoload 'json-mode "json-mode" t)
;; (add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

;;==============================================================================
;; Open all headers with C++mode
;;==============================================================================
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cxx\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))


(provide 'mode-mappings)
