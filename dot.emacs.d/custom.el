(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-brace-indent-level 4)
 '(TeX-source-correlate-method 'synctex)
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(TeX-view-program-list '(("Okular" "okular --unique %o#src:%n%b")))
 '(TeX-view-program-selection
   '((output-pdf "Okular")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "xpdf")
     (output-html "xdg-open")))
 '(a2ps-switches
   '("-b" "-l 90" "-T 4" "-M A4" "--colums=1" "-R" "--line-numbers=1") t)
 '(auto-image-file-mode t nil (image-file))
 '(case-fold-search t)
 '(clang-format-executable "clang-format")
 '(column-number-mode t)
 '(current-language-environment "German")
 '(custom-safe-themes
   '("6cfe5b2f818c7b52723f3e121d1157cf9d95ed8923dbc1b47f392da80ef7495d" default))
 '(default-frame-alist '((menu-bar-lines . 1) (width . 132) (heigth . 55)))
 '(default-input-method "german-postfix")
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(frame-background-mode 'dark)
 '(global-font-lock-mode t nil (font-lock))
 '(isearch-allow-scroll t)
 '(mouse-wheel-mode t nil (mwheel))
 '(package-archives
   '(("melpa-stable" . "https://stable.melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/")))
 '(package-check-signature nil)
 '(package-selected-packages
   '(terraform-mode kubernetes mu4e-overview qml-mode go-dlv go-mode json-mode json-reformat json-snatcher vlf jsonnet-mode ## json json-navigator dts-mode indent-tools yaml-mode matlab-mode markdown-mode fill-column-indicator elpy cmake-ide))
 '(ring-bell-function 'ignore)
 '(select-enable-clipboard t)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(uniquify-min-dir-content 1)
 '(user-mail-address "thomas@ruschival.de")
 '(words-include-escapes t))

(load-theme 'tsdh-dark t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "Black" :foreground "White" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "1ASC" :family "Liberation Mono"))))
 '(cursor ((t (:background "khaki"))))
 '(font-lock-builtin-face ((((class color) (background dark)) (:foreground "Turquoise"))))
 '(font-lock-comment-face ((t (:foreground "wheat"))))
 '(font-lock-constant-face ((((class color) (background dark)) (:bold t :foreground "DarkOrchid"))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face :bold t))))
 '(font-lock-doc-string-face ((t (:foreground "green2"))))
 '(font-lock-function-name-face ((t (:foreground "SkyBlue"))))
 '(font-lock-keyword-face ((t (:bold t :foreground "CornflowerBlue"))))
 '(font-lock-preprocessor-face ((t (:italic nil :foreground "CornFlowerBlue"))))
 '(font-lock-reference-face ((t (:foreground "DodgerBlue"))))
 '(font-lock-string-face ((t (:foreground "LimeGreen"))))
 '(font-lock-type-face ((t (:foreground "CornFlowerBlue"))))
 '(font-lock-variable-name-face ((t (:foreground "PaleGreen"))))
 '(font-lock-warning-face ((((class color) (background dark)) (:foreground "yellow" :background "red"))))
 '(header-line ((((class color grayscale) (background light)) (:inherit mode-line :foreground "grey20" :box nil))))
 '(makefile-space ((t (:background "wheat"))))
 '(makefile-space-face ((t (:background "wheat"))) t)
 '(mode-line ((t (:background "dim gray"))))
 '(paren-match ((t (:background "darkseagreen4"))))
 '(quote (cursor ((t (:background "#A5F5FA")))))
 '(region ((t (:background "dark slate blue"))))
 '(scroll-bar ((t (:background "lightgrey" :foreground "black"))))
 '(show-paren-match ((t (:background "yellow" :foreground "black"))))
 '(show-paren-mismatch ((t (:background "red" :foreground "white" :weight bold))))
 '(whitespace-space ((t (:foreground "dark salmon")))))
