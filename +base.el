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
(setq inhibit-compacting-font-caches t
      read-process-output-max (* 16 1024 1024)
      gc-cons-threshold (* 2 1024 1024 1024)  ; 2GB
      gc-cons-percentage 0.6
      doom-gc-cons-threshold (* 2 1024 1024 1024)
      doom-gc-cons-upper-limit (* 4 1024 1024 1024)
      doom-gc-cons-percentage 0.5)

(after! gcmh
  (setq gcmh-idle-delay 30          ; 更频繁的GC
        gcmh-auto-idle-delay-factor 2
        gcmh-high-cons-threshold (* 2 1024 1024 1024)
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
(when (file-exists-p custom-file)
  (message "Custom file detected. Use config.el and +ui.el for customizations instead."))
