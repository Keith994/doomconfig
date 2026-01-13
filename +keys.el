;;; +keys.el -*- lexical-binding: t; -*-

;; ============================================================================
;; Leader Key Bindings
;; ============================================================================
(map! :leader
      ;; Debug
      (:prefix ("d" . "debug")
       :desc "dape breakpoint toggle" "b" #'+my/dape-breakpoint-toggle
       :desc "dape breakpoint remove all" "B" #'+my/dape-breakpoint-remove-all)
      
      ;; Search
      :desc "Search project" "s g" #'+default/search-project
      
      ;; Version Control
      :desc "Revert git buffer" "g h r" #'+vc-gutter/revert-hunk)

;; ============================================================================
;; Global Key Bindings
;; ============================================================================
(map!
 ;; Navigation
 :n ";" #'execute-extended-command
 :n "H" #'evil-beginning-of-line
 :n "L" #'evil-end-of-line
 "M-`" #'other-frame

 ;; Lookup & Documentation
 :n "gy" #'+lookup/documentation
 :n "gR" #'lsp-rename
 :nv "gr" #'+lookup/references
 :nv "gi" #'+lookup/implementations
 :nv "gD" #'xref-find-definitions-other-window
 :nv "gl" #'flycheck-display-error-at-point

 ;; Convenience
 :nv ",c" #'+my/smart-close-window-enhanced
 :nv ",w" #'save-buffer
 :nv ",f" #'+my/lsp-formatter
 :nv ",p" #'display-which-path

 ;; Version Control Gutter
 :n "]g" #'+vc-gutter/next-hunk
 :n "[g" #'+vc-gutter/previous-hunk

 ;; Basic Editing
 :ni "C-k" #'kill-line
 :ni "C-n" #'next-line
 :ni "C-p" #'previous-line)

;; ============================================================================
;; Mode-specific Key Bindings
;; ============================================================================
(map!
 ;; Programming Mode
 (:map prog-mode-map
  :i "TAB" #'doom/dumb-indent
  :i "<backtab>" #'doom/dumb-dedent
  :n "ga" #'lsp-execute-code-action
  :n "]e" #'flycheck-next-error
  :n "[e" #'flycheck-previous-error)

 ;; Dirvish (File Manager)
 (:after dirvish
  (:map dirvish-mode-map
   :n "f" #'find-file
   :n "F" #'dirvish-file-info-menu
   :n "TAB" #'dired-mark
   :n "m" #'dirvish-subtree-toggle
   :n "yr" #'dirvish-copy-file-relative-path
   :n "yy" #'dired-copy-filename-as-kill))

 ;; LSP UI
 (:after lsp-ui
  :map lsp-ui-mode-map
  "C-j" #'lsp-ui-doc-mode)

 ;; LSP Peek
 (:after lsp-ui-peek
  :map lsp-ui-peek-mode-map
  "h" #'lsp-ui-peek--select-prev-file
  "j" #'lsp-ui-peek--select-next
  "k" #'lsp-ui-peek--select-prev
  "l" #'lsp-ui-peek--select-next-file)

 ;; Go Mode
 (:after go-ts-mode
  (:map go-ts-mode-map
   :localleader
   (:prefix "t"
    "y" #'+go/copy-go-test-dlv-cmd
    "Y" #'+go/copy-go-test-run-cmd
    (:prefix ("B" . "bench")
     "s" #'+go/bench-single
     "a" #'+go/bench-all))))

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

 ;; Corfu (Completion)
 (:after corfu-mode
  (:map corfu-mode-map
   :ni "C-n" nil
   :ni "C-p" nil)))

;; ============================================================================
;; Aliases
;; ============================================================================
(defalias 'delete-window-alias 'delete-window)

