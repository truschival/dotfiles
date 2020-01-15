;;==============================================================================
;; Menu-customization (do not edit by hand) in separate file
;; https://www.emacswiki.org/emacs/CustomFile
;;==============================================================================
(setq custom-file
	  (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(setq settings-dir
	  (expand-file-name "settings" user-emacs-directory))


;;==============================================================================
;; Other customizations
;;==============================================================================
(let ((default-directory "~/.emacs.d/elpa"))
  (normal-top-level-add-subdirs-to-load-path))

;; Set up load path
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path "~/.emacs.d/local")

;;(load "~/.emacs.d/emacs_keybindings")
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(set-background-color "Black")
(set-foreground-color "White")
(set-cursor-color "khaki")
(set-mouse-color "khaki")

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
(setq tab-width 4)					;; set your desired tab width
(setq default-tab-width 4)			;; set your desired tab width
(setq indent-tabs-mode nil)			;; may use tabs, space if nil
(setq make-backup-files nil)		;; no backups

(which-func-mode t)					;; Show function in mode-line
(tool-bar-mode 0)					;; No tool-bar
(ruler-mode t)						;; Ruler line on top
(transient-mark-mode t)				;; Highlight selection
(delete-selection-mode 1)			;; delete seleted text when typing


;; Show matching parens (mixed style)
(show-paren-mode t)
(setq show-paren-delay 0.0)
(setq show-paren-style 'parenthesis)


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
  (if (eolp) (let (parens-require-spaces) (insert-pair)) (self-insert-command 1)))


;;==============================================================================
;; MATLAB
;;==============================================================================
(autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
(add-to-list
 'auto-mode-alist
 '("\\.m$" . matlab-mode))
(setq matlab-indent-function t)
(setq matlab-shell-command "matlab")
(setq matlab-shell-command-switches '("-nodesktop -nosplash"))


;;==============================================================================
;; Line numbers
;;==============================================================================
(global-linum-mode 1)
(setq linum-format "%4d|")

;; Whitespace-style
(setq whitespace-style '(trailing lines space-before-tab
								  indentation space-after-tab)
	  whitespace-line-column 80)
(global-whitespace-mode t)


;;==============================================================================
;; Fill column Indicator
;;==============================================================================
(require 'fill-column-indicator)
(setq fci-rule-color "#FFAA00")
(setq fci-rule-width 1)
(setq fci-rule-use-dashes t)
(setq fci-dash-pattern 0.6)
(setq-default fill-column 80)
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)

;;============
;; Aspell
;;============
;;(add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
(setq ispell-program-name "aspell")
(setq text-mode-hook '(lambda() (flyspell-mode t) ))


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

  (require 'rtags)
  (require 'company)
  (require 'flycheck)
  (require 'flycheck-rtags)

  (setq rtags-completions-enabled t)
  (setq rtags-autostart-diagnostics t)
  (rtags-enable-standard-keybindings)

  ;; Staryt CMake-IDE
  (cmake-ide-setup)

  (setq-default c-basic-offset 4
				tab-width 4
				indent-tabs-mode t)
  (setq tab-width 4)
  )
(add-hook 'c-mode-hook 'my-c-hook)
(add-hook 'c++-mode-hook 'my-c-hook)


;;==============================================================================
;; Flycheck + Company-mode autocomplete
;;==============================================================================

(defun setup-flycheck-rtags ()
  (interactive)
  (flycheck-select-checker 'rtags)
  ;; RTags creates more accurate overlays.
  (setq-local flycheck-highlighting-mode nil)
  (setq-local flycheck-check-syntax-automatically nil))

;; only run this if rtags is installed
(when (require 'rtags nil :noerror)
  ;; make sure you have company-mode installed
  (require 'company)
  (define-key c-mode-base-map (kbd "M-.")
	(function rtags-find-symbol-at-point))
  (define-key c-mode-base-map (kbd "M-,")
	(function rtags-find-references-at-point))
  ;; install standard rtags keybindings. Do M-. on the symbol below to
  ;; jump to definition and see the keybindings.
  (rtags-enable-standard-keybindings)
  ;; company completion setup
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  (setq rtags-completions-enabled t)
  (push 'company-rtags company-backends)
  (global-company-mode)
  (define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))
  ;; use rtags flycheck mode -- clang warnings shown inline
  (require 'flycheck-rtags)
  ;; c-mode-common-hook is also called by c++-mode
  (add-hook 'c-mode-common-hook #'setup-flycheck-rtags))


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


;; enable highligh-symbols
(require 'highlight-symbol)
;; Some minor helpers
(require 'custom-lisp)
;; Map files to modes
(require 'mode-mappings)
;; Load key-mappings
(require 'key-mappings)




;;;
;; TeX configs
;;;
;;(require 'tex-site)
;; ;(eval-after-load "tex"
;;   '(add-to-list 'TeX-command-list
;;                 '("make acronyms" "makeindex -s %s.ist  -o %s.acn %s.acr" TeX-run-command nil t) t))
;; (eval-after-load "tex"
;;   '(add-to-list 'TeX-command-list
;;                 '("make glossary" "makeindex -s %s.ist  -o %s.gls %s.glo" TeX-run-command nil t) t)
;; )



;; Pull in individual customizations for modes etc. each in a separate file
;;(when (file-exists-p user-settings-dir)
;;  (mapc 'load (directory-files user-settings-dir nil "^[^#].*el$")))
