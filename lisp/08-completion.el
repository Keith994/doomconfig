;; 08-completion.el --- Completion Framework Configuration -*- lexical-binding: t -*-

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-component-separator #'orderless-escapable-split-on-space))

;; VERTical Interactive COmpletion
(use-package vertico
  :custom (vertico-count 15)
  :bind (:map vertico-map
          ("RET" . vertico-directory-enter)
          ("DEL" . vertico-directory-delete-char)
          ("M-DEL" . vertico-directory-delete-word))
  :hook ((after-init . vertico-mode)
          (rfn-eshadow-update-overlay . vertico-directory-tidy)))

;; Display vertico in the child frame
(use-package vertico-posframe
  :functions posframe-poshandler-frame-center-near-bottom
  :hook (vertico-mode . vertico-posframe-mode)
  :init (setq vertico-posframe-poshandler
              #'posframe-poshandler-frame-center-near-bottom
              vertico-posframe-parameters
              '((left-fringe  . 8)
                (right-fringe . 8))))

;; Enrich existing commands with completion annotations
(use-package marginalia
  :hook (after-init . marginalia-mode))

;; Add icons to completion candidates
(use-package nerd-icons-completion
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

;; Consulting completing-read
(use-package consult
  :defines (xref-show-xrefs-function xref-show-definitions-function)
  :defines shr-color-html-colors-alist
  :autoload (consult-register-format consult-register-window consult-xref)
  :autoload (consult--read consult--customize-put)
  :commands (consult-narrow-help)
  :functions (list-colors-duplicates consult-colors--web-list)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)))

(use-package embark
    :commands embark-prefix-help-command
    :bind (("C-."   . embark-act)
           :map minibuffer-local-map
           ("M-." . my-embark-preview))
    :init
    ;; Optionally replace the key help with a completing-read interface
    (setq prefix-help-command #'embark-prefix-help-command)
    :config
    ;; Manual preview for non-Consult commands using Embark
    (defun my-embark-preview ()
      "Previews candidate in vertico buffer, unless it's a consult command."
      (interactive)
      (unless (bound-and-true-p consult--preview-function)
        (save-selected-window
          (let ((embark-quit-after-action nil))
            (embark-dwim)))))

    ;; Hide the mode line of the Embark live/completions buffers
    (add-to-list 'display-buffer-alist
                 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   nil
                   (window-parameters (mode-line-format . none))))

    (with-no-warnings
      (with-eval-after-load 'which-key
        (defun embark-which-key-indicator ()
          "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
          (lambda (&optional keymap targets prefix)
            (if (null keymap)
                (which-key--hide-popup-ignore-command)
              (which-key--show-keymap
               (if (eq (plist-get (car targets) :type) 'embark-become)
                   "Become"
                 (format "Act on %s '%s'%s"
                         (plist-get (car targets) :type)
                         (embark--truncate-target (plist-get (car targets) :target))
                         (if (cdr targets) "…" "")))
               (if prefix
                   (pcase (lookup-key keymap prefix 'accept-default)
                     ((and (pred keymapp) km) km)
                     (_ (key-binding prefix 'accept-default)))
                 keymap)
               nil nil t (lambda (binding)
                           (not (string-suffix-p "-argument" (cdr binding))))))))

        (setq embark-indicators
              '(embark-which-key-indicator
                embark-highlight-indicator
                embark-isearch-highlight-indicator))

        (defun embark-hide-which-key-indicator (fn &rest args)
          "Hide the which-key indicator immediately when using the completing-read prompter."
          (which-key--hide-popup-ignore-command)
          (let ((embark-indicators
                 (remq #'embark-which-key-indicator embark-indicators)))
            (apply fn args)))

        (advice-add #'embark-completing-read-prompter
                    :around #'embark-hide-which-key-indicator))))

(use-package embark-consult
  :ensure t
  :bind (:map minibuffer-mode-map
          ("C-o" . embark-export)))
  
;; Auto completion
(use-package corfu
  :autoload (corfu-quit consult-completion-in-region)
  :functions (persistent-scratch-save corfu-move-to-minibuffer)
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-count 12)
  (corfu-preview-current nil)
  (corfu-on-exact-match nil)
  (corfu-auto-delay 0.2)
  (corfu-popupinfo-delay '(0.4 . 0.2))
  (global-corfu-modes '((not erc-mode
                              circe-mode
                              help-mode
                              gud-mode
                              vterm-mode)
                        t))
  :custom-face
  (corfu-border ((t (:inherit region :background unspecified))))
  :bind ("<TAB>" . completion-at-point)
  :hook ((after-init . global-corfu-mode)
          (global-corfu-mode . corfu-popupinfo-mode)
          (global-corfu-mode . corfu-history-mode))
  :config
  ;;Quit completion before saving
  (add-hook 'before-save-hook #'corfu-quit)
  (advice-add #'persistent-scratch-save :before #'corfu-quit)

  ;; Move completions to minibuffer
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
        (let ((completion-extra-properties extras)
              completion-cycle-threshold completion-cycling)
          (consult-completion-in-region beg end table pred)))))
  (keymap-set corfu-map "M-m" #'corfu-move-to-minibuffer)
  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer))

(unless (or (display-graphic-p) (featurep 'tty-child-frames))
  (use-package corfu-terminal
    :hook (global-corfu-mode . corfu-terminal-mode)))


(use-package nerd-icons-corfu
  :autoload nerd-icons-corfu-formatter
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Add extensions
(use-package cape
  :commands (cape-file cape-elisp-block cape-keyword)
  :autoload (cape-wrap-noninterruptible cape-wrap-nonexclusive cape-wrap-buster)
  :autoload (cape-wrap-silent)
  :init
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; (add-to-list 'completion-at-point-functions #'cape-abbrev)

  ;; Make these capfs composable.
  (advice-add 'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
  (advice-add 'lsp-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add 'comint-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive)))

(provide '08-completion)
