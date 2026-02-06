;;; +ui.el -*- lexical-binding: t; -*-

;; ============================================================================
;; Theme & Appearance
;; ============================================================================
(setq catppuccin-flavor 'mocha
      doom-theme 'doom-dracula
      +workspaces-on-switch-project-behavior 'non-empty)
(setq doom-font "Maple Mono NF CN-13.5")
;;(set-fontset-font "fontset-default" 'han (font-spec :family "WenQuanYi Micro Hei Mono"))

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
 doom-modeline--eglot t
 doom-modeline-buffer-encoding t
 doom-modeline-window-width-limit (- fill-column 10)
 doom-modeline-vcs-max-length 80
 doom-modeline-major-mode-icon t
 doom-modeline-enable-word-count t
 doom-modeline-buffer-file-name-style 'file-name-with-project
 doom-modeline-minor-modes nil)
