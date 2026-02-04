;;; +ui.el -*- lexical-binding: t; -*-

(after! doom-init-ui
  (set-language-environment "English")
  (set-default-coding-systems 'utf-8)
  (set-buffer-file-coding-system 'utf-8)
  (set-clipboard-coding-system 'utf-8)
  (set-file-name-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-next-selection-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (prefer-coding-system 'utf-8))
;; ============================================================================
;; Theme & Appearance
;; ============================================================================
(setq doom-font (font-spec :family "Maple Mono NF CN" :size 16 :weight 'regular)
      catppuccin-flavor 'mocha
      doom-theme 'doom-dracula
      +workspaces-on-switch-project-behavior 'non-empty)

;; ============================================================================
;; Window & Buffer Management
;; ============================================================================
;; (set-popup-rules! '(("^\\*helpful" :size 0.35)
;;                     ("^\\*Ibuffer\\*$" :size 0.35)
;;                     ("^\\*info.*" :size 80 :side right)
;;                     ("^\\*Man.*" :size 80 :side right)
;;                     ("^\\*keycast.*" :size 50 :side right)
;;                     ("^\\*Customize" :actions display-buffer)
;;                     ("^\\*edit-indirect" :size 0.6)
;;                     ("^\\*YASnippet Tables\\*$" :size 0.35)
;;                     ("^\\*grep\\*$" :size 0.35)
;;                     ("^\\*pytest\\*" :size 0.35)
;;                     ("^\\*Anaconda\\*$" :size 0.35)
;;                     ("^\\*aider.*$" :ignore t)
;;                     ("\\*Async Shell Command\\*$" :side bottom :size 0.30 :select t)
;;                     ("\\*.*server log\\*$" :side top :size 0.20 :select nil)
;;                     ((lambda (buf _) (with-current-buffer buf (eq major-mode 'forge-topic-mode))) :size 0.35)))
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

(use-package! hl-todo
  :hook (prog-mode . hl-todo-mode)
  :init
  ;; code here will run immediately
  :config
  ;; code here will run after the package is loaded
  (setq hl-todo-highlight-punctuation ":"))


;; ============================================================================
;; Doom Modeline
;; ============================================================================
(setq
 doom-modeline-icon t
 doom-modeline-window-width-limit (- fill-column 10)
 doom-modeline-vcs-max-length 80
 doom-modeline-major-mode-icon t
 doom-modeline-enable-word-count t
 doom-modeline-buffer-file-name-style 'file-name-with-project
 doom-modeline-minor-modes nil)
