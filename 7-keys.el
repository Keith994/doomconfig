;;; 7-keys.el -*- lexical-binding: t; -*-


(map! :leader
      (:prefix ("d" . "debug")
        :desc "dape breakpoint toggle" "b" #'+my/dape-breakpoint-toggle
        :desc "dape breakpoint remove all" "B" #'+my/dape-breakpoint-remove-all
        ))

(map!
 :nv "gr" #'+lookup/references
 :nv "gi" #'+lookup/implementations
 :nv "gD" #'xref-find-definitions-other-window
 :nv ",c" #'delete-window
 :nv ",w" #'save-buffer
 :nv ",f" #'format-all-region-or-buffer
 :nv ",p" #'display-which-path
 "M-`"   #'other-frame
 )

(map!
 (:map prog-mode-map
  :i "TAB" #'doom/dumb-indent
  :i "<backtab>" #'doom/dumb-dedent
  :n "ga" #'lsp-execute-code-action)

 (:after dirvish
         (:map dirvish-mode-map
          :n "f" #'find-file
          :n "F" #'dirvish-file-info-menu
          :n "TAB" #'dired-mark
          :n "m" #'dirvish-subtree-toggle
          :n "yr" #'dirvish-copy-file-relative-path
          :n "yy" #'dired-copy-filename-as-kill))
 (:after lsp-ui
  :map lsp-ui-mode-map
  "C-j" #'lsp-ui-doc-mode)
 (:after lsp-ui-peek
  :map lsp-ui-peek-mode-map
  "h" #'lsp-ui-peek--select-prev-file
  "j" #'lsp-ui-peek--select-next
  "k" #'lsp-ui-peek--select-prev
  "l" #'lsp-ui-peek--select-next-file)
 (:after go-ts-mode
         (:map go-ts-mode-map
          :localleader
          (:prefix "t"
                   "y" #'+go/copy-go-test-dlv-cmd
                   "Y" #'+go/copy-go-test-run-cmd
                   (:prefix ("B" . "bench")
                            "s" #'+go/bench-single
                            "a" #'+go/bench-all))))
 (:after evil-vars
         (:map evil-window-map
          :leader
          (:prefix "w"
           :desc "evil-window-decrease-height" "-" (cmd! (evil-window-decrease-height 10))
           :desc "evil-window-increase-height" "+" (cmd! (evil-window-increase-height 10))
           :desc "evil-window-decrease-width" "<"  (cmd! (evil-window-decrease-width 20))
           :desc "evil-window-increase-width" ">"  (cmd! (evil-window-increase-width 20)))))
 (:when (modulep! :completion vertico)
   (:after vertico
    :map vertico-map
    "C-j" nil "C-k" nil
    "C-j"   #'+vertico/embark-preview
    "C-n"   #'vertico-next
    "C-M-n" #'+vertico/next-candidate-preview
    "C-S-n" #'vertico-next-group
    "C-p"   #'vertico-previous
    "A-v"   #'vertico-scroll-down
    "C-v"   #'vertico-scroll-up
    "C-M-p" #'+vertico/previous-candidate-preview
    "C-S-p" #'vertico-previous-group))
 (:after minibuffer
  :map minibuffer-local-map
  (:when (modulep! :completion vertico)
    "M-RET" #'vertico-exit-input)
  "C-t" #'marginalia-cycle
  "C-k" #'kill-line)
 (:after magit-mode
         (:map magit-mode-map "M-p" nil "M-n" nil "M-w" nil
          :nv "$" #'magit-process-buffer
          "C-c r" #'code-review-forge-pr-at-point))
 (:after magit-diff
         (:map magit-diff-mode-map            ; for magit diff/rev mode
               "C-o" #'magit-diff-visit-file-other-window))
 (:after magit-blame
         (:map magit-blame-mode-map
          :n "o" #'magit-blame--git-link-commit))
 )
