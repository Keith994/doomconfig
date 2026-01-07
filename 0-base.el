;;; 0-base.el -*- lexical-binding: t; -*-

;; User information
(setq user-full-name "Keith Woo"
      user-mail-address "keithwoo1994@163.com")

;; The catppuccin theme
(setq catppuccin-flavor 'mocha) ;; or 'latte, 'macchiato, or 'mocha
(setq doom-theme 'catppuccin)

;; Font settings
(setq doom-font (font-spec :name "Maple Mono NF CN" :size 14))

;; org directory
(setq org-directory "~/org/")

;; Scratch buffer settings
(setq doom-scratch-buffer-major-mode 'emacs-lisp-mode
      confirm-kill-emacs nil)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Manually edit .local/custom.el will break doom updates
(when (file-directory-p custom-file)
  (message (concat "Please delete " custom-file ". And customization in config.el and ui.el.")))

;; Default settings
(setq-default fill-column 120
              delete-trailing-lines t
              vterm-shell "/bin/fish"
              explicit-shell-file-name "/bin/fish")

;; Delete the selection when pasting
(delete-selection-mode 1)

;; disable risky local variables warning
(advice-add 'risky-local-variable-p :override #'ignore)

;; Global Auto-Revert Mode is a global minor mode that reverts any
;; buffer associated with a file when the file changes on disk
(global-auto-revert-mode t)

;; Suppress specific warnings
(custom-set-variables
  '(warning-suppress-log-types '((lsp-mode) (iedit))) ; Suppress lsp-mode and iedit warnings in the log
  '(warning-suppress-types '((iedit)))                ; Suppress iedit warnings
  '(warning-suppress-log-types '((lsp-mode) (iedit))) ; Suppress lsp-mode and iedit warnings in the log
  '(warning-suppress-types '((iedit))))               ; Suppress iedit warnings

;; delete to trash
(setq delete-by-moving-to-trash t)

;; doom modeline settings
(setq doom-modeline-vcs-max-length 80
      doom-modeline-buffer-file-name-style 'relative-from-project
      doom-modeline-icon t
      doom-modeline-major-mode-icon t
      doom-modeline-enable-word-count t)
