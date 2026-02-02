;;; +keys.el -*- lexical-binding: t; -*-

;; ============================================================================
;; Windows Navigation Enhancements
;; ============================================================================
(map! :leader
      :n "1" #'winum-select-window-1
      :n "2" #'winum-select-window-2
      :n "3" #'winum-select-window-3
      :n "4" #'winum-select-window-4)

;; ============================================================================
;; Leader Key Bindings
;; ============================================================================
(map! :leader
      ;; Search
      :desc "Search project" "s g" #'+default/search-project
      ;; Version Control
      :desc "Revert git buffer" "g h r" #'+vc-gutter/revert-hunk
      :desc "Dired" "o d" #'dirvish)

;; ============================================================================
;; Global Key Bindings
;; ============================================================================
(map!
 ;; Navigation
 :n ";" #'execute-extended-command
 :n "H" #'evil-beginning-of-line
 :n "L" #'evil-end-of-line
 :n "K" #'centaur-tabs-forward
 :n "J" #'centaur-tabs-backward
 "M-`" #'other-frame

 ;; Lookup & Documentation
 :n "gy" #'+lookup/documentation
 :n "gR" #'lsp-rename
 :nv "gr" #'+lookup/references
 :nv "gi" #'+lookup/implementations
 :nv "gD" #'xref-find-definitions-other-window
 :nv "gl" #'flymake-show-diagnostic

 ;; Convenience
 :nv ",c" #'+my/smart-close-window-enhanced
 :nv ",w" #'save-buffer
 :nv ",f" #'+my/formatter
 :nv ",p" #'display-which-path

 ;; Version Control Gutter
 :n "]g" #'+vc-gutter/next-hunk
 :n "[g" #'+vc-gutter/previous-hunk

 ;; Basic Editing
 :ni "C-k" #'kill-line
 :ni "C-n" #'next-line
 :ni "C-p" #'previous-line)

;; ============================================================================
;; Dirvish (File Manager) Key Bindings
;; ============================================================================
(map!
 (:after dirvish
         (:map dirvish-mode-map
          :n "f" #'find-file
          :n "F" #'dirvish-file-info-menu
          :n "TAB" #'dired-mark
          :n "m" #'dirvish-subtree-toggle
          :n "yf" #'dirvish-copy-file-name
          :n "yr" #'dirvish-copy-file-relative-path
          :n "yy" #'dired-copy-filename-as-kill)))

;; ============================================================================
;; Mode-specific Key Bindings
;; ============================================================================
(map!
 ;; Programming Mode
 (:map prog-mode-map
  :i "TAB" #'doom/dumb-indent
  :i "<backtab>" #'doom/dumb-dedent
  :n "ga" #'lsp-execute-code-action
  :n "]e" #'flymake-goto-next-error
  :n "[e" #'flymake-goto-prev-error)


 ;; Window Management
 (:after evil-vars
         (:map evil-window-map
          :leader
          (:prefix "w"
           :desc "evil-window-decrease-height" "-" (cmd! (evil-window-decrease-height 10))
           :desc "evil-window-increase-height" "+" (cmd! (evil-window-increase-height 10))
           :desc "evil-window-decrease-width" "<" (cmd! (evil-window-decrease-width 20))
           :desc "evil-window-increase-width" ">" (cmd! (evil-window-increase-width 20)))))

 ;; Vertico (Completion)
 (:when (modulep! :completion vertico)
   (:after vertico
    :map vertico-map
    "C-j" nil
    "C-k" nil
    "C-." #'embark-act
    "C-j" #'+vertico/embark-preview
    "C-n" #'vertico-next
    "C-M-n" #'+vertico/next-candidate-preview
    "C-S-n" #'vertico-next-group
    "C-p" #'vertico-previous
    "A-v" #'vertico-scroll-down
    "C-v" #'vertico-scroll-up
    "C-M-p" #'+vertico/previous-candidate-preview
    "C-S-p" #'vertico-previous-group))

 ;; Minibuffer
 (:after minibuffer
  :map minibuffer-local-map
  (:when (modulep! :completion vertico)
    "M-RET" #'vertico-exit-input)
  "C-t" #'marginalia-cycle
  "C-k" #'kill-line)

 ;; Magit
 (:after magit-mode
         (:map magit-mode-map
          "M-p" nil "M-n" nil "M-w" nil
          :nv "$" #'magit-process-buffer
          "C-c r" #'code-review-forge-pr-at-point))

 (:after magit-diff
         (:map magit-diff-mode-map
               "C-o" #'magit-diff-visit-file-other-window))

 (:after magit-blame
         (:map magit-blame-mode-map
          :n "o" #'magit-blame--git-link-commit))

 )

;; ============================================================================
;; Aliases
;; ============================================================================
(defalias 'delete-window-alias 'delete-window)

