;;; +use-package.el -*- lexical-binding: t; -*-

;; ============================================================================
;; Navigation & Editing
;; ============================================================================

;; Evil Mode Configuration
(setq evil-cross-lines t
      evil-split-window-below t
      evil-vsplit-window-right t
      evil-ex-substitute-global t)

(after! evil
  (evil-define-text-object evil-inner-buffer (count &optional beg end type)
    (list (point-min) (point-max)))
  (define-key evil-inner-text-objects-map "g" 'evil-inner-buffer))

;; Evil Snipe
(after! evil-snipe
  (setq evil-snipe-scope 'buffer
        evil-snipe-repeat-scope 'buffer)
  (push 'prodigy-mode evil-snipe-disabled-modes))

;; Evil Escape
(use-package! evil-escape
  :config
  (setq evil-escape-key-sequence "jk"
        evil-escape-delay 0.2)
  (evil-escape-mode 1))

;; Better Jumper
(add-hook! 'better-jumper-post-jump-hook #'recenter)

;; ============================================================================
;; File Management
;; ============================================================================

;; Dirvish (Enhanced Dired)
(after! dirvish
  (setq dirvish-attributes
        '(vc-state file-size nerd-icons collapse subtree-state file-time))
  (setq dirvish-quick-access-entries
        `(("h" "~/" "Home")
          ("c" "~/.config" "config")
          ("d" "~/repo" "dev")
          ("g" "~/repo/github.com/" "github")
          ("D" "~/Downloads" "Downloads")
          ("e" ,doom-user-dir "Doom directory")
          ("E" ,doom-emacs-dir "Emacs directory")))
  
  (setq dirvish-hide-details '(dired dirvish dirvish-side)
        dirvish-hide-cursor '(dired dirvish dirvish-side))
  
)

;; IBuffer Enhancement
(after! ibuffer
  (setq-hook! 'ibuffer-hook ibuffer-formats
              '((mark modified read-only locked " "
                 (name 50 18 :left :elide)
                 " "
                 (size 9 -1 :right)
                 " "
                 (mode 16 16 :left :elide)
                 " " filename-and-process)
                (mark " "
                      (name 16 -1)
                      " " filename))))

(use-package! all-the-icons-ibuffer
  :after ibuffer
  :init (all-the-icons-ibuffer-mode 1))

;; Imenu List
(use-package! imenu-list
  :ensure t
  :defer t
  :config
  (set-popup-rules! '(("^\\*Ilist\\*" :side right :size 40 :select t))))

;; ============================================================================
;; Completion & UI
;; ============================================================================

;; Vertico Completion
(when (modulep! :completion vertico)
  (setq vertico-posframe-border-width 3)
  (setq vertico-posframe-poshandler #'posframe-poshandler-frame-bottom-center)
  
  ;; Fix jump issue for vertico
  (dolist (func '(+default/search-project))
    (advice-add func :around #'doom-set-jump-a)))

;; Corfu Completion
(after! corfu
  (setq corfu-preselect t)
  (map! :map corfu-mode-map
        :ni "C-n" nil
        :ni "C-p" nil))

;; ============================================================================
;; Terminal & Shell
;; ============================================================================

;; Term Mode
(after! term
  (add-hook! 'term-mode-hook (setq-local imenu-generic-expression '(("Prompt" "➜\\(.*\\)" 1)))))

;; Process Menu
(add-hook! 'process-menu-mode-hook
  (setq-local tabulated-list-format [("Process" 30 t)
                                     ("PID"      7 t)
                                     ("Status"   7 t)
                                     ("Buffer"  15 t)
                                     ("TTY"     12 t)
                                     ("Command"  0 t)]))

;; ============================================================================
;; AI & Coding Assistance
;; ============================================================================

;; Aider (AI Coding Assistant)
(use-package! aider
  :config
  (setq aider-args '("--model" "deepseek/deepseek-coder"))
  (require 'aider-doom))

;; Comint Mode Settings
(set-evil-initial-state! '(comint-mode) 'insert)
(add-hook! 'comint-mode-hook #'visual-line-mode)

;; ============================================================================
;; Debugging & Development
;; ============================================================================

;; Dape Debugger Configuration
(when (modulep! :tools debugger)
  (defun +my/dape-breakpoint-toggle ()
    (interactive)
    (require 'dape)
    (dape-breakpoint-toggle)
    (+go/write-project-breakpoints))

  (defun +my/dape-breakpoint-remove-all ()
    (interactive)
    (require 'dape)
    (dape-breakpoint-remove-all)
    (+go/write-project-breakpoints))

  (after! dape
    (setq dape-configs (assq-delete-all 'dlv dape-configs))
    (add-to-list 'dape-configs
                 `(delve
                   modes (go-mode go-ts-mode)
                   ensure dape-ensure-command
                   fn (dape-config-autoport dape-config-tramp)
                   command "dlv"
                   command-args ("dap" "--listen" "127.0.0.1::autoport")
                   command-insert-stderr t
                   command-cwd (lambda()(if (string-suffix-p "_test.go" (buffer-name))
                                            default-directory (dape-cwd)))
                   port :autoport
                   :type "debug"
                   :request "launch"
                   :mode (lambda() (if (string-suffix-p "_test.go" (buffer-name)) "test" "debug"))
                   :program "."
                   :cwd "."
                   :args (lambda()
                           (if (string-suffix-p "_test.go" (buffer-name))
                               (save-excursion
                                 (when (re-search-backward "^func[ \t]+\\(\\(\\w\\|\\s_\\)+\\)" nil t)
                                   (let* ((test-name (match-string 1))
                                          (test-regexp (concat "^" test-name "$")))
                                     `["-test.run" ,test-regexp])))
                             []))))
    ))

;; (use-package! spring-boot-mode
;;   :hook (java-ts-mode . spring-boot-mode))
(after! projectile
        ;; 只用 .git 作为项目根判定依据
        (setq projectile-project-root-files '(".git")
        projectile-project-root-files-bottom-up '(".git")
        projectile-project-root-files-top-down-recurring '(".git")))

;; ============================================================================
;; Language Support
;; ============================================================================

;; Protobuf Mode
(use-package! protobuf-mode :defer t)

;; Which Function
(use-package! which-func :defer t :commands which-function)

;; ============================================================================
;; Utilities
;; ============================================================================

;; Screenshot
(use-package! screenshot :defer t)

;; Consult Todo
(use-package! consult-todo :ensure t :defer t)

;; Keycast
(use-package! keycast :ensure t :defer t)

;; (use-package! rime
;;               :custom
;;               (rime-user-data-dir "~/.local/share/fcitx5/rime/")
;;               (rime-show-candidate 'minibuffer))

;; Compilation Mode
(add-hook! compilation-mode #'visual-line-mode)
