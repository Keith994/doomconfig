;;; +base.el -*- lexical-binding: t; -*-

;; ============================================================================
;; User Information
;; ============================================================================
(setq user-full-name "Keith Woo"
      user-mail-address "keithwoo1994@163.com")

;; ============================================================================
;; Core Settings
;; ============================================================================
(setq org-directory "~/org/"
      doom-scratch-buffer-major-mode 'emacs-lisp-mode
      confirm-kill-emacs nil
      display-line-numbers t
      display-line-numbers-type 'relative
      delete-by-moving-to-trash t)

;; ============================================================================
;; Editor Behavior
;; ============================================================================
(setq-default fill-column 120
              delete-trailing-lines t
              vterm-shell "/bin/fish"
              explicit-shell-file-name "/bin/fish")

(delete-selection-mode 1)
(global-auto-revert-mode t)

;; ============================================================================
;; Performance Optimizations
;; ============================================================================
(setq +lsp--optimization-init-p t
      +lsp--default-read-process-output-max (* 16 1024 1024)
      +lsp--default-gcmh-high-cons-threshold (* 4 1024 1024 1024)
      inhibit-compacting-font-caches t
      read-process-output-max (* 16 1024 1024)
      gc-cons-threshold (* 1 1024 1024 1024)
      gc-cons-percentage 0.7
      doom-gc-cons-threshold (* 1 1024 1024 1024)
      doom-gc-cons-upper-limit (* 1 1024 1024 1024)
      doom-gc-cons-percentage 0.6)

(after! gcmh
  (setq gcmh-idle-delay 60
        gcmh-auto-idle-delay-factor 3
        gcmh-high-cons-threshold (* 1 1024 1024 1024)
        gcmh-verbose nil))

;; ============================================================================
;; Warning Suppression
;; ============================================================================
(advice-add 'risky-local-variable-p :override #'ignore)
(custom-set-variables
  '(warning-suppress-log-types '((lsp-mode) (iedit)))
  '(warning-suppress-types '((iedit))))

;; ============================================================================
;; Clipboard (Wayland Support)
;; ============================================================================
(when (and (eq system-type 'gnu/linux)
           (getenv "WAYLAND_DISPLAY")
           (executable-find "wl-copy"))
  (defun wl-copy-selection (str)
    "Copy STR to Wayland clipboard using wl-copy."
    (with-temp-buffer
      (insert str)
      (call-process-region (point-min) (point-max) "wl-copy" t nil t)))

  (defun wl-paste-selection ()
    "Paste from Wayland clipboard using wl-paste."
    (let ((default-directory "~"))
      (string-trim (shell-command-to-string "wl-paste --no-newline"))))

  (setq interprogram-cut-function 'wl-copy-selection
        interprogram-paste-function 'wl-paste-selection)
  (message "Configured Wayland clipboard support via wl-clipboard."))

;; ============================================================================
;; Custom File Warning
;; ============================================================================
(when (file-directory-p custom-file)
  (message (concat "Please delete " custom-file ". And customization in config.el and ui.el.")))
