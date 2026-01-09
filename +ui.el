;;; +ui.el -*- lexical-binding: t; -*-

;; Font settings
(setq doom-font (font-spec :family "Maple Mono NF CN" :size 14))

;; The catppuccin theme
(setq catppuccin-flavor 'mocha) ;; or 'latte, 'macchiato, or 'mocha
(setq doom-theme 'catppuccin)

;; doom-init-ui-hook
(remove-hook 'doom-init-ui-hook #'blink-cursor-mode)

;; modeline settings
(after! doom-modeline
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project
        doom-modeline-major-mode-icon t
        ;; My mac vsplit screen won't fit
        doom-modeline-window-width-limit (- fill-column 10)))

(setq +workspaces-on-switch-project-behavior t)

(defface breakpoint-enabled '((t)) "Breakpoint face.")

;; for terminal
(unless (display-graphic-p)
  (custom-set-faces!
    `(mode-line-inactive :background ,(doom-darken (doom-color 'bg-alt) 0.05) :foreground ,(doom-color 'fg))))
