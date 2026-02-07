;;; +base.el -*- lexical-binding: t; -*-

;; 设置 Emacs 使用 utf-8 作为默认编码
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)

;; 禁用 BOM
(setq utf-8-with-signature-coding-system 'utf-8)

(setq tab-width 4
      mark-ring-max 128
      global-mark-ring-max 128)
(blink-cursor-mode 1)

;; ============================================================================
;; User Information
;; ============================================================================
(setq user-full-name "Keith Woo"
      user-mail-address "keithwoo1994@163.com")

;; ============================================================================
;; Core Settings
;; ============================================================================
(setq org-directory "~/orgs/"
      tab-width 2
      confirm-kill-emacs nil
      display-line-numbers t
      display-line-numbers-type 'relative
      delete-by-moving-to-trash t)


;; ============================================================================
;; Editor Behavior
;; ============================================================================
(setq-default fill-column 120
              delete-trailing-lines t)

(delete-selection-mode 1)
(global-auto-revert-mode t)

;; ============================================================================
;; Performance Optimizations
;; ============================================================================
(setq inhibit-compacting-font-caches t
      read-process-output-max (* 16 1024 1024)
      gc-cons-threshold (* 500 1024 1024)  ; 500MB
      gc-cons-percentage 0.6)

(after! gcmh
  (setq gcmh-idle-delay 60          ; 更频繁的GC
        gcmh-auto-idle-delay-factor 0.3
        gcmh-high-cons-threshold (* 500  1024 1024)
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
