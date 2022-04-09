;;==============================================================================
;; Menu-customization (do not edit by hand) in separate file
;; https://www.emacswiki.org/emacs/CustomFile
;;==============================================================================
(setq custom-file
	  (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(setq settings-dir
	  (expand-file-name "settings" user-emacs-directory))

(setq custom-theme-directory
      (expand-file-name "themes" user-emacs-directory))

;;==============================================================================
;; Other customizations
;;==============================================================================
(let ((default-directory "~/.emacs.d/elpa"))
  (normal-top-level-add-subdirs-to-load-path))

;; Set up load path
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path "~/.emacs.d/local")

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Garbage collection at 20MB
(setq gc-cons-threshold 20000000)

;; Fix ^H / Delete keybinding issues
(normal-erase-is-backspace-mode 0)
(when (window-system)
	  (normal-erase-is-backspace-mode t))
;; Always start Server if not running
(load "server")
 (unless (server-running-p) (server-start))

;; Don't insert instructions in the *scratch* buffer
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq frame-title-format "%b")
(setq tab-width 4)			;; set your desired tab width
(setq default-tab-width 4)		;; set your desired tab width
(setq indent-tabs-mode nil)		;; may use tabs, space if nil

(which-func-mode t)			;; Show function in mode-line
(tool-bar-mode 0)			;; No tool-bar
(ruler-mode 0)				;; Ruler line on top
(transient-mark-mode t)			;; Highlight selection
(delete-selection-mode 1)		;; delete seleted text when typing
(display-time-mode t)

;; Show matching parens (mixed style)
(show-paren-mode t)
(setq show-paren-delay 0.0)
(setq show-paren-style 'parenthesis)

;; backup files in /tmp
(setq make-backup-files nil)		;; no backups
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Initialize windmove default to Shift-<arrow> keys
(windmove-default-keybindings )

;;==============================================================================
;; Completion of buffer names in switching
;;==============================================================================
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(defun ido-ignore-non-user-except-ielm (name)
  "Ignore all non-user (a.k.a. *starred*) buffers except *shell*."
  (and (string-match "^\*" name)
	   (not (string= name "*shell*"))))
(setq ido-ignore-buffers '("\\` " ido-ignore-non-user-except-ielm))

;;==============================================================================
;; Magic parentesis and ""
;;==============================================================================
(electric-pair-mode 0)
(defun electric-pair ()
  "If at end of line, insert character pair without surrounding spaces.
	Otherwise, just insert the typed character."
  (interactive)
  (if (eolp)
	  (let (parens-require-spaces) (insert-pair))
	(self-insert-command 1)
 ))

;;==============================================================================
;; Line numbers
;;==============================================================================
;; before 26.0.50 use linum-mode
;; (global-linum-mode 1)
;; (setq linum-format "%4d|")
(global-display-line-numbers-mode)

;;==============================================================================
;; Fill column Indicator
;;==============================================================================
(require 'fill-column-indicator)
(setq
 fci-rule-color "#FFAA00"
 fci-rule-width 1
 fci-rule-use-dashes t
 fci-dash-pattern 0.7)
(setq-default fill-column 80)
(define-globalized-minor-mode global-fci-mode fci-mode
  (lambda () (fci-mode 1)))
(global-fci-mode 1)

;;============
;; Aspell
;;============
;;(add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
;; Use aspell if installed
(when (executable-find "aspell")
  (setq ispell-program-name "aspell")
  (setq ispell-list-command "--list"))
(setq text-mode-hook '(lambda() (flyspell-mode t) ))
(setq flyspell-issue-message-flag nil)

;;============
;; Ediff
;;============
(setq ediff-split-window-function (if (> (frame-width) 150)
				      'split-window-horizontally
				    'split-window-vertically))
;;============
;; Easy editing lisp
;; not polluting other sources with lisp auto-complete
;;============
(defun my-lisp-hook()
  (setq ac-sources '(ac-source-symbols
		     ac-source-variables
		     ac-source-functions
		     ac-source-features
		     )
	)
  )
(add-hook 'emacs-lisp-mode-hook 'my-lisp-hook)

;;==============================================================================
;; Golang mode
;;==============================================================================
(defun my-go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save)
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-,") 'pop-tag-mark)
)
(add-hook 'go-mode-hook 'my-go-mode-hook)

;;==============================================================================
;; Python
;;==============================================================================
;;(package-initialize)
;;(elpy-enable)

;;==============================================================================
;; Unique Buffer Names
;;==============================================================================
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;==============================================================================
;; CC-Mode and hooks
;;==============================================================================
(defun my-c-hook()
  (load "clang-format.el")
  (global-set-key [C-M-f] 'clang-format-region)
  (global-set-key [f6] 'clang-format-buffer)

  (setq-default c-basic-offset 4
		tab-width 4
		indent-tabs-mode t)
  (setq tab-width 4)
  )
(add-hook 'c-mode-hook 'my-c-hook)
(add-hook 'c++-mode-hook 'my-c-hook)


;;============
;; Ctags
;;============
(setq path-to-ctags "/usr/bin/ctags-exuberant")
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "ctags-exuberant -e -R %s"  (directory-file-name dir-name)))
  )

;; Some minor helpers
(require 'custom-lisp)
;; Map files to modes
(require 'mode-mappings)
;; Load key-mappings
(require 'key-mappings)
;; Load mu4e settings
(require 'email-mu4e)

;; Pull in individual customizations for modes etc. each in a separate file
;;(when (file-exists-p user-settings-dir)
;;  (mapc 'load (directory-files user-settings-dir nil "^[^#].*el$")))
