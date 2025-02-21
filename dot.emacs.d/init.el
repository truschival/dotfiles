;;==============================================================================
;; Configuration files and directories
;;==============================================================================

;; No menu-customizations permanent in a file.
;; All persistent customizations are made here in init.el
(setq custom-file (make-temp-file "emacs-custom"))

(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))

(setq local-modes-dir
      (expand-file-name "local" user-emacs-directory))

;; Create elpa package dir if it does not exist
(defvar elpa-dir
  (expand-file-name "elpa" user-emacs-directory)
  "directory for elpa packages")
(make-directory elpa-dir :parents)

(let ((default-directory elpa-dir))
  (normal-top-level-add-subdirs-to-load-path))

;; Set up load path
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path local-modes-dir)

(add-to-list 'default-frame-alist '(width . 88))

;;==============================================================================
;; Automatic package installation
;;==============================================================================
(require 'package)
;; Initializes the package infrastructure
(setq package-check-signature nil
      package-enable-at-startup nil)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(when (not package-archive-contents)
  (package-refresh-contents))

;; other packages are installed by use-package :ensure t)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

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

;; Set Frame (Window) Title
(setq frame-title-format '("" "Emacs v" emacs-version " [%b]"))

(setq-default tab-width 4)      ;; set your desired tab width
(setq default-tab-width 4)      ;; set your desired tab width
(setq indent-tabs-mode 0)       ;; may use tabs, space if nil
(setq isearch-allow-scroll t)   ;; allow scroll during isearch (C-s)
(setq-default scroll-preserve-screen-position 'always) ;; do not move cursor
;; Suppress native compile warning buffers popping up
(setq
 ;; native-comp-async-report-warnings-errors nil
 warning-minimum-level :error)

(tool-bar-mode 0)               ;; No tool-bar
(ruler-mode 0)                  ;; no ruler line on top
(transient-mark-mode t)         ;; Highlight selection
(column-number-mode t)          ;; show column in mode bar
(delete-selection-mode 1)       ;; delete seleted text when typing
(global-display-line-numbers-mode) ;; show line numbers everywhere

;; Only show time/date in terminal mode
(unless (display-graphic-p)
  (display-time-mode 1)
  (setq
   display-time-24hr-format t
   display-time-day-and-date t
   )
  )

;; please follow symlinks to git files without asking
(setq vc-follow-symlinks t)

;; No backups, stale locks, autosaves with old content etc.
(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Open file in dired buffer and kill previous
(put 'dired-find-alternate-file 'disabled nil)

;; Ediff left<->right if frame large enough
(setq ediff-split-window-function
	  (if (> (frame-width) 150)
          'split-window-horizontally
        'split-window-vertically))

;; Pull in individual customizations for modes etc. each in a separate file
(when (file-exists-p settings-dir)
  (mapc 'load (directory-files settings-dir nil "^[^#].*el$")))

;;==============================================================================
;; Packages
;;==============================================================================

;;-------------------------------
;; Show matching parentheses
(use-package paren
  :ensure t
  :custom
  (show-paren-mode t)
  (show-paren-delay 0)
  (show-paren-stype 'parenthesis)
  )

;;-------------------------------
;; Unique Buffer Names
(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-min-dir-content 1)			;; show at least last directory
)

;;-------------------------------
;; Help me by showing possible hotkey combinations
(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-popup-type 'side-window)
  )

;;-------------------------------
;; Completion of buffer names in switching
(use-package ido
  :ensure t
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
  (setq ido-use-filename-at-point 'guess)
  (setq ido-use-url-at-point 0)
  (setq ido-create-new-buffer 'always) ;; Do not ask to create new buffer
  )

;;-------------------------------
;; Smart parenthesis - significant upgrade from electric pair mode
(use-package smartparens
  :ensure t
  :hook
  (prog-mode . smartparens-mode)
  (text-mode . smartparens-mode)
  )

;;-------------------------------
;; Fill column Indicator
;; Emacs >26 includes display-fill-column-indicator that superseeds fci-mode
(use-package display-fill-column-indicator
  :ensure t
  :config
  (setq-default fill-column 80)
  :hook
  (prog-mode . display-fill-column-indicator-mode)
  (text-mode . display-fill-column-indicator-mode)
  )

;;-------------------------------
;; Aspell
;;(add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
;; Use aspell if installed
(when (executable-find "aspell")
  (use-package flyspell
    :ensure t
    :custom
    (ispell-program-name "aspell")
    (flyspell-issue-message-flag nil)
    (ispell-list-command "--list")
    :hook
    (text-mode . flyspell-mode)
    (markdown-mode . flyspell-mode)
    )
  )

;;-------------------------------
;; auth sources and secrets for freedesktop secrets (keepassxc)
(use-package auth-source
  :init
  (setq     auth-source-debug nil)
  )


(use-package secrets
  :init
  (setq
   auth-sources '(default
                  "secrets:"
                  ;; "secrets:session"
                  ;; "secrets:Login"
                  )
   auth-source-save-behavior nil
   secrets-debug nil
   )
  )


;; (let ((auth-info (car (auth-source-search :host "cloud.ruschival.de" :user "ruschi" :port 443))))
;;   (funcall (plist-get auth-info :secret))
;;   )
;; Freedesktop secrets "Secret-service-API" - the default collection is ""
;; (secrets-list-collections)
;; (secrets-list-items "" )
;; (secrets-search-items "" :host "cloud.ruschival.de" :port "443")
;; (secrets-get-secret "" "ORG-CALDAV")


;;-------------------------------
;; Caldav-sync
(use-package org-caldav
  :ensure t
  :config
  (setq org-caldav-url "https://cloud.ruschival.de/remote.php/dav/calendars/ruschi")
  (setq org-caldav-calendar-id "org-mode")
  (setq org-caldav-files  '("~/Nextcloud/calendar/calendar.org") )
  (setq org-icalendar-timezone "Europe/Berlin")
  (setq org-caldav-debug-level 0)
  (setq org-caldav-show-sync-results nil) ;; 0, 1 ,2
  (setq org-caldav-disable-sync-buffer t)
  ;; Start timer after package is loaded
  (defun my/safe-caldav-sync ()
  (condition-case err
      (org-caldav-sync)
    (error (message "Error in org-caldav-sync: %s" err))))
  (run-at-time "1 min" (* 15 60) 'my/safe-caldav-sync)
  )



;;==============================================================================
;; Language Server stuff
;;==============================================================================
(use-package lsp-mode
  :ensure t
  :init
  ;;set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-idle-delay 0.1)
  (setq lsp-modeline-code-actions-segments '(count icon name))
  (setq lsp-auto-guess-root t)
  (setq lsp-log-io nil)						;; log if non-nil
  (setq lsp-enable-snippet nil)				;; not using yasippet
  (setq lsp-restart 'auto-restart)
  (setq lsp-modeline-code-actions-enable t)
  (setq lsp-modeline-diagnostics-enable t)
  (setq lsp-diagnostic-clean-after-change t)
  (setq lsp-enable-imenu t)
  (setq lsp-pylsp-plugins-black-enabled t)
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  ;; enable / disable the hints as you prefer:
  (setq lsp-inlay-hint-enable t)
  :hook ((c-mode
		  c++-mode
		  go-mode
		  python-mode
		  rustic-mode)
         . lsp-deferred)
  ;; if you want which-key integration
  (lsp-mode . lsp-enable-which-key-integration)
  :commands (lsp lsp-deferred)
  )

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-delay 0.5)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-peek-enable t)
  )

(use-package company
  :ensure t
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind
  ;; :bind (("<s-SPC>" . company-complete))
  (:map company-active-map
        ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.1)
  (setq company--disabled-backends '(company-dabbrev
									 company-dabbrev-code
									 company-bbdb
									 company-capf))
  )


(use-package helm
  :ensure t
  :config
  (setq helm-M-x-fuzzy-match t)
  :bind
  ("M-x" . helm-M-x)
)


(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol
  )

;; Follow cross-references (defined, implementation... M-. M-, )
(use-package helm-xref
  :ensure t
  )

(use-package lsp-treemacs
  :ensure t
  :after lsp
  :commands (lsp-treemacs-errors-list lsp-treemacs-symbols)
  )
(use-package exec-path-from-shell
  :ensure
  :init (exec-path-from-shell-initialize))

;;==============================================================================
;; language/file specific major/minor modes
;;==============================================================================

;;-------------------------------
;; org-mode
(use-package org
  :ensure t
  :config
  (progn
    (setq org-ctrl-k-protect-subtree t) ;; avoid killing subtrees
    (setq org-todo-keywords
          '((sequence "TODO(t)" "STARTED(s)" "BLOCKED(b)" "|" "DONE" "INVALID")))
    (setq org-completion-use-ido t)
	(visual-line-mode 1)
    )
  )

;;-------------------------------
;; Golang mode
(use-package go-mode
  :ensure t
  :bind (:map go-mode-map
         ("M-." . 'godef-jump)
         ("M-," . 'pop-tag-mark))
  :hook
  (before-save . gofmt-before-save)
  )

;;-------------------------------
;; CMake
(use-package cmake-mode
  :ensure t
  :custom
  (cmake-tab-width 4)
  )

;;-------------------------------
;; Yaml
(use-package yaml-mode
  :ensure t
  :custom
  (yaml-indent-offset 2)
  )

;;-------------------------------
;; Markdown
(use-package markdown-mode
  :ensure t
  )

;;-------------------------------
;; JSON-Mode
(use-package json-mode
  :ensure t
  )

;;-------------------------------
;; Rust-Mode (use rustic)
(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)
  (setq rustic-format-on-save t)
  (setq indent-tabs-mode nil)
  :hook
  (prettify-symbols-mode)
)

;;-------------------------------
;; Python
(use-package python
  :ensure t
  :config
  (setq  fill-column 88) ;; black formatter defaults to 88
  ;; (elpy-enable)
  )

;; load pyvenv to switch venvs and restart lsp
(use-package pyvenv
  :ensure t
  :hook (python-mode . pyvenv-mode)
  )

;;-------------------------------
;; C++ Programming Mode and hooks
(use-package clang-format
  :ensure t
  )

(use-package cc-mode
  :bind (
		 :map c-mode-map
		 ("C-M-f" . 'clang-format-region)
		 ("<f6>"  . 'clang-format-buffer)
		 :map c++-mode-map
		 ("C-M-f" . 'clang-format-region)
		 ("<f6>"  . 'clang-format-buffer)
		 )
  )

;;==============================================================================
;; Themes and UI
;;==============================================================================
(load-theme 'tsdh-dark t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default
	((t
	  (:inherit nil :stipple nil :background "Black"
				:foreground "White" :inverse-video nil :box nil
				:strike-through nil :overline nil :underline nil
				:slant normal :weight normal
				:height 122 :width normal :foundry "SRC" :family "Hack"))))
 '(cursor ((t (:background "khaki"))))
 '(font-lock-builtin-face
   ((((class color) (background dark)) (:foreground "Turquoise"))))
 '(font-lock-comment-face ((t (:foreground "wheat"))))
 '(font-lock-constant-face
   ((((class color) (background dark)) (:bold t :foreground "DarkOrchid"))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face :bold t))))
 '(font-lock-doc-string-face ((t (:foreground "green2"))))
 '(font-lock-function-name-face ((t (:foreground "SkyBlue"))))
 '(font-lock-keyword-face ((t (:bold t :foreground "CornflowerBlue"))))
 '(font-lock-preprocessor-face ((t (:italic nil :foreground "CornFlowerBlue"))))
 '(font-lock-reference-face ((t (:foreground "DodgerBlue"))))
 '(font-lock-string-face ((t (:foreground "LimeGreen"))))
 '(font-lock-type-face ((t (:foreground "CornFlowerBlue"))))
 '(font-lock-variable-name-face ((t (:foreground "PaleGreen"))))
 '(font-lock-warning-face
   ((((class color) (background dark))
	 (:foreground "yellow" :background "red"))))
 '(header-line
   ((((class color grayscale) (background light))
	 (:inherit mode-line :foreground "grey20" :box nil))))
 '(makefile-space ((t (:background "wheat"))))
 '(makefile-space-face ((t (:background "wheat"))) t)
 '(mode-line ((t (:background "dim gray"))))
 '(paren-match ((t (:background "darkseagreen4"))))
 '(quote (cursor ((t (:background "#A5F5FA")))))
 '(region ((t (:background "dark slate blue"))))
 '(scroll-bar ((t (:background "lightgrey" :foreground "black"))))
 '(show-paren-match ((t (:background "yellow" :foreground "black"))))
 '(show-paren-mismatch
   ((t (:background "red" :foreground "white" :weight bold))))
 '(whitespace-space ((t (:foreground "dark salmon"))))
 '(lsp-inlay-hint-face ((t (:foreground "thistle4" :slant italic))))
 )

;;---------- EOF ----------
