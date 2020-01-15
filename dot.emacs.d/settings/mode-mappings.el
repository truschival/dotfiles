;;====================================================================
;; Map file-names to modes
;;====================================================================


;;
;; EDD-Mode
;;
(autoload 'edd-mode "edd-mode"
  "Electronic Device Description Language" t)
(add-to-list 'auto-mode-alist '("\\.dd$" . edd-mode))
(add-to-list 'auto-mode-alist '("\\.ddl$" . edd-mode))
(add-to-list 'auto-mode-alist '("\\.edd$" . edd-mode))

;;
;; VHDL mode
;;
(autoload 'vhdl-mode "vhdl-mode" "VHDL Mode" t)
(add-to-list 'auto-mode-alist '("\\.vhd$" . vhdl-mode))
(add-to-list 'auto-mode-alist '("\\.vhdl$" . vhdl-mode))

;;
;; CMake Mode
;;
(autoload 'cmake-mode "cmake-mode" t)
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt$" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake$" . cmake-mode))

;;
;; QML
;;
(autoload 'qml-mode "qml-mode" "Editing Qt Declarative." t)
(add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))

;; Snippets
(add-to-list 'auto-mode-alist '("yasnippet/snippets" . snippet-mode))
(add-to-list 'auto-mode-alist '("\\.yasnippet$" . snippet-mode))

;;
;; Open all headers with C++mode
;;
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cxx\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))


(provide 'mode-mappings)
