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
(package! hide-mode-line)
(package! minions)
(package! catppuccin-theme)
(package! all-the-icons-ibuffer)
(package! keycast)
(package! symbol-overlay)

;; ============================================================================
;; Editing & Navigation
;; ============================================================================
(package! evil-escape)
(package! evil-string-inflection)
(package! imenu-list)
(package! consult-todo)

;; ============================================================================
;; File & Project Management
;; ============================================================================
(package! git-link)
(package! magit-delta)
(package! tmux-pane)

;; ============================================================================
;; Language Support
;; ============================================================================
(package! lsp-java)
(package! bazel)
(package! jinja2-mode)
(package! protobuf-mode)
(package! gn-mode)

;; LSP Configuration
(if (modulep! :tools lsp +eglot)
    (progn
      (package! breadcrumb :recipe (:host github :repo "joaotavora/breadcrumb"))
      (package! eglot-java))
  (package! lsp-docker))
;(package! spring-boot-mode)

;; ============================================================================
;; AI & Coding Assistance
;; ============================================================================
(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))
(package! aider :recipe (:host github :repo "tninja/aider.el"))

;; ============================================================================
;; Text & Documentation
;; ============================================================================
(package! adoc-mode)
(package! tldr)
(package! djvu)
(package! blog-admin :recipe (:host github :repo "codefalling/blog-admin"))
(package! pomm)
(package! org-appear)

;; ============================================================================
;; System Integration
;; ============================================================================
(package! atomic-chrome)
;; (package! rime :recipe (:host github :repo "DogLooksGood/emacs-rime" :files ("*.el" "Makefile" "lib.c")))
(package! xclip)

;; ============================================================================
;; Screenshot
;; ============================================================================
(package! screenshot)
