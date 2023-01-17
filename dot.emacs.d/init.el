;;==============================================================================
;; Configuration files and directories
;;==============================================================================

;; No menu-customizations permanent in a file.
;; All persistent customizations are made here in init.el
(setq custom-file (make-temp-file "emacs-custom"))

(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))

;; Create elpa package dir if it does not exist
(defvar elpa-dir
  (expand-file-name "elpa" user-emacs-directory)
  "directory for elpa packages")
(make-directory elpa-dir :parents)

(let ((default-directory elpa-dir))
    (normal-top-level-add-subdirs-to-load-path))

;; Set up load path
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path "~/.emacs.d/local")

;;==============================================================================
;; Automatic package installation
;;==============================================================================
(require 'package)
;; Initializes the package infrastructure
(setq  package-check-signature nil
       package-enable-at-startup nil)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (not package-archive-contents)
  (package-refresh-contents))

(setq  myPackages
       '(
         (clang-format)
         (fill-column-indicator)
         (markdown-mode)
         (json-mode)
         (terraform-mode)
         (go-mode)
         (pyenv-mode)
         (elpy)
         (use-package)
         (helm-lsp)
         (helm-xref)
         (lsp-mode)
         (lsp-treemacs)
         (lsp-ui)
         (treemacs)
         (which-key)
         )
       )
(dolist (p (mapcar 'car myPackages))
  (unless (package-installed-p p)
    (package-install p)))

;;==============================================================================
;; Other customizations
;;==============================================================================
(setq
 ;; Garbage collection at 20MB
 gc-cons-threshold 20000000
 ;; Increase amount of data read from a process
 read-process-output-max (* 1024 1024))

;; Always start Server if not running
(load "server")
(unless (server-running-p) (message "starting emacs server")(server-start))

(set-language-environment "UTF-8")
(setq-default user-mail-address "thomas@ruschival.de")
(setq-default user-full-name "Thomas Ruschival")

;; Don't insert instructions in the *scratch* buffer
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default ring-bell-function 'ignore) ;; no visual/audible bell

(setq frame-title-format '("" "Emacs v" emacs-version " [%b]"))
(setq-default tab-width 4)      ;; set your desired tab width
(setq default-tab-width 4)      ;; set your desired tab width
(setq indent-tabs-mode 0)       ;; may use tabs, space if nil
(tool-bar-mode 0)           ;; No tool-bar
(ruler-mode 0)              ;; no ruler line on top
(transient-mark-mode t)     ;; Highlight selection
(delete-selection-mode 1)   ;; delete seleted text when typing
(global-display-line-numbers-mode) ;; show line numbers everywhere
(column-number-mode t) ;; show column in mode bar
(setq isearch-allow-scroll t) ;; allow scroll during isearch (C-s)

;; Only show time/date in terminal mode
(unless (display-graphic-p)
  (display-time-mode 1)
  (setq
   display-time-24hr-format t
   display-time-day-and-date t
   )
  )

;; backup files in /tmp
(setq make-backup-files nil)        ;; no backups
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Open file in dired buffer and kill previous
(put 'dired-find-alternate-file 'disabled nil)

;; Initialize windmove default to Shift-<arrow> keys
(windmove-default-keybindings 'shift)

;; Pull in individual customizations for modes etc. each in a separate file
(when (file-exists-p settings-dir)
  (mapc 'load (directory-files settings-dir nil "^[^#].*el$")))

;;==============================================================================
;; Themes and UI
;;==============================================================================
(load-theme 'tsdh-dark t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "Black" :foreground "White" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 122 :width normal :foundry "SRC" :family "Hack"))))
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

;;==============================================================================
;; individual package configs
;;==============================================================================
(use-package paren
  :custom
  (show-paren-mode t)
  (show-paren-delay 0)
  (show-paren-stype 'parenthesis)
  )

(use-package which-key
  :custom
  (which-key-mode)
  )

(use-package lsp-mode
  :init
  ;;set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-idle-delay 0.1)
  (setq lsp-prefer-flymake nil)
  :hook ((c-mode . lsp)
         (c++-mode . lsp)
         (go-mode . lsp)
         (python-mode . lsp)
         ;; if you want which-key integration
         ;; (lsp-mode . lsp-enable-which-key-integration)
         )
  :commands (lsp lsp-deferred)
  )

(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.1)
)

(use-package helm-lsp
  :commands helm-lsp-workspace-symbol
  )

(use-package lsp-ui
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-delay 1.0)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-doc-position 'bottom)
  (setq lsp-ui-peek-enable t)
  )

(use-package lsp-treemacs
  :after lsp
  :commands lsp-treemacs-errors-list)

;;==============================================================================
;; Completion of buffer names in switching
;;==============================================================================
(use-package ido
  :config
  (ido-mode 1)
  (progn (defun ido-ignore-non-user-except-ielm (name)
           "Ignore all non-user (a.k.a. *starred*) buffers except *shell*."
           (and (string-match "^\*" name)
                (not (string= name "*shell*"))))
         (setq ido-ignore-buffers '("\\` " ido-ignore-non-user-except-ielm))
         )
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  )

;;==============================================================================
;; Magic parentesis and "" - "electric pair"
;;==============================================================================
(use-package elec-pair
  :config
  (electric-pair-mode 1)
  (progn (defun electric-pair ()
           "If at end of line, insert character pair without surrounding spaces.
    Otherwise, just insert the typed character."
           (interactive)
           (if (eolp)
               (let (parens-require-spaces) (insert-pair))
             (self-insert-command 1)
             ))
         )
  :diminish
  )

;;==============================================================================
;; org-mode settings
;;==============================================================================
(use-package org
  :config
  (progn
    (setq org-ctrl-k-protect-subtree t) ;; avoid killing subtrees
    (setq org-todo-keywords
          '((sequence "TODO(t)" "STARTED(s)" "BLOCKED(b)"  "|" "DONE" "INVALID")))
    (setq org-completion-use-ido t)
    (visual-line-mode 1)
    )
  )

;;==============================================================================
;; Fill column Indicator
;;==============================================================================
(use-package display-fill-column-indicator
  :config
  (setq-default fill-column 80)
  :hook
  (prog-mode . display-fill-column-indicator-mode)
  (text-mode . display-fill-column-indicator-mode)
  )


;;============
;; Aspell
;;============
;;(add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
;; Use aspell if installed
(when (executable-find "aspell")
  (use-package flyspell
    :ensure nil
    :custom
    (ispell-program-name "aspell")
    (flyspell-issue-message-flag nil)
    (ispell-list-command "--list")
    :hook
    (text-mode . flyspell-mode)
    (markdown-mode . flyspell-mode)
    )
  )

;;============
;; Ediff
;;============
(setq ediff-split-window-function (if (> (frame-width) 150)
                                      'split-window-horizontally
                                    'split-window-vertically))

;;==============================================================================
;; Golang mode
;;==============================================================================
(use-package go-mode
  :bind (
         ("M-." . 'godef-jump)
         ("M-," . 'pop-tag-mark))
  :hook
  (before-save . gofmt-before-save)
  )

;;==============================================================================
;; Python
;;==============================================================================
(use-package python
  :config
  (elpy-enable)
  (setq python-shell-interpreter "/usr/bin/pipenv"
        python-shell-interpreter-args " run ipython -i --simple-prompt")
  )

;; (defun my-python-hook()
;;   (elpy-enable)
;;   (pyenv-mode)
;;   (setq python-shell-interpreter "/usr/bin/pipenv"
;;         python-shell-interpreter-args " run ipython -i --simple-prompt")
;;   )
;; (add-hook 'python-mode-hook 'my-python-hook)

;;==============================================================================
;; Unique Buffer Names
;;==============================================================================
(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-min-dir-content 1)
  )

;;==============================================================================
;; CC-Mode and hooks
;;==============================================================================
(use-package clang-format
  :config
  (defun map-clang-format-keys()
    (local-set-key (kbd "C-M-f") 'clang-format-region)
    (local-set-key (kbd "<f6>")  'clang-format-buffer)
    (message "loaded map-clang-format-keys")
    )
  :hook (
         (c-mode . map-clang-format-keys)
         (c++-mode . map-clang-format-keys)
         )
  )
