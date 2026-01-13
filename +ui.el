;;; +ui.el -*- lexical-binding: t; -*-

;; ============================================================================
;; Theme & Appearance
;; ============================================================================
(setq doom-font (font-spec :family "Maple Mono NF CN" :size 14)
      catppuccin-flavor 'mocha
      doom-theme 'catppuccin
      +workspaces-on-switch-project-behavior t)

;; ============================================================================
;; Doom Modeline
;; ============================================================================
(after! doom-modeline
  (setq doom-modeline-vcs-max-length 80
        doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-enable-word-count t
        doom-modeline-window-width-limit (- fill-column 10)))

;; ============================================================================
;; Window & Buffer Management
;; ============================================================================
(set-popup-rules! '(("^\\*helpful" :size 0.35)
                    ("^\\*Ibuffer\\*$" :size 0.35)
                    ("^\\*info.*" :size 80 :side right)
                    ("^\\*Man.*" :size 80 :side right)
                    ("^\\*keycast.*" :size 50 :side right)
                    ("^\\*Customize" :actions display-buffer)
                    ("^\\*edit-indirect" :size 0.6)
                    ("^\\*YASnippet Tables\\*$" :size 0.35)
                    ("^\\*grep\\*$" :size 0.35)
                    ("^\\*pytest\\*" :size 0.35)
                    ("^\\*Anaconda\\*$" :size 0.35)
                    ("^\\*aider.*$" :ignore t)
                    ("\\*Async Shell Command\\*$" :side bottom :size 0.30 :select t)
                    ("\\*.*server log\\*$" :side top :size 0.20 :select nil)
                    ((lambda (buf _) (with-current-buffer buf (eq major-mode 'forge-topic-mode))) :size 0.35)))

;; Aider specific window placement
(after! aider
  (add-to-list 'display-buffer-alist
              '((major-mode . #'aider-comint-mode)
                (display-buffer-in-side-window)
                (side . right)
                (slot . 0)
                (window-width . 0.4))))

;; ============================================================================
;; Syntax Highlighting & Colors
;; ============================================================================
(defface breakpoint-enabled '((t)) "Breakpoint face.")

;; HL-Todo configuration
(custom-theme-set-faces! doom-theme
  `(hl-todo :foreground ,(doom-color 'bg)))

(after! hl-todo
  (setq hl-todo-color-background t
        hl-todo-keyword-faces
        `(("TODO"  . ,(doom-color 'orange))
          ("HACK"  . ,(doom-color 'orange))
          ("TEMP"  . ,(doom-color 'orange))
          ("DONE"  . ,(doom-color 'green))
          ("NOTE"  . ,(doom-color 'green))
          ("DONT"  . ,(doom-color 'red))
          ("DEBUG"  . ,(doom-color 'red))
          ("FAIL"  . ,(doom-color 'red))
          ("FIXME" . ,(doom-color 'red))
          ("XXX"   . ,(doom-color 'blue))
          ("XXXX"  . ,(doom-color 'blue)))))

;; ============================================================================
;; Terminal-specific Settings
;; ============================================================================
(unless (display-graphic-p)
  (custom-set-faces!
    `(mode-line-inactive :background ,(doom-darken (doom-color 'bg-alt) 0.05) :foreground ,(doom-color 'fg)))
  (setq evil-insert-state-cursor 'bar))
