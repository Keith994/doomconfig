;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; ============================================================================
;; Disabled Packages
;; ============================================================================
(disable-packages! solaire-mode
                   osx-trash
                   realgud
                   realgud-trepan-ni
                   ccls
                   tide
                   swiper
                   forge
                   code-review
                   writegood-mode
                   dired-x
                   flymake-popon
                   anaconda-mode
                   company-anaconda
                   lsp-python-ms
                   pyimport)

;; ============================================================================
;; UI & Appearance
;; ============================================================================
(package! catppuccin-theme)
(package! all-the-icons-ibuffer)
(package! keycast)
(package! multiple-cursors)

;; ============================================================================
;; AI & Coding Assistance
;; ============================================================================
(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))
(package! aider :recipe (:host github :repo "tninja/aider.el"))
(unpin! :completion :ui :editor :emacs :checkers :lang :tools)
