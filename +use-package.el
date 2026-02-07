;;; +use-package.el -*- lexical-binding: t; -*-

;; Corfu Completion
(use-package! corfu
  :custom
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-preselect 'prompt)          ;; 不预选任何候选项
  ;; (corfu-preselect 'prompt)   ;; 预选输入提示（默认）
  ;; (corfu-preselect 'first)    ;; 预选第一个候选项
  ;; (corfu-preselect 'valid)    ;; 只在有精确匹配时预选
  (corfu-quit-no-match 'separator)
  :bind
  (:map corfu-map
        ("RET" . nil))
  )

(use-package! multiple-cursors)

(after! consult
  (setq project--list nil))


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

;; ============================================================================
;; Completion & UI
;; ============================================================================

;; Vertico Completion
(setq vertico-posframe-border-width 1)
(setq vertico-posframe-poshandler #'posframe-poshandler-frame-bottom-center)
;; Fix jump issue for vertico
(dolist (func '(+default/search-project))
  (advice-add func :around #'doom-set-jump-a))

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
(add-hook! 'comint-mode-hook #'visual-line-mode)

;; ============================================================================
;; Debugging & Development
;; ============================================================================

;; Dape Debugger Configuration
(when (modulep! :tools debugger)
  (setq dape-breakpoint-margin-string "")
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

;; ============================================================================
;; Utilities
;; ============================================================================

;; Consult Todo
(use-package! consult-todo :defer t)

;; Keycast
(use-package! keycast :ensure t :defer t)

;; Enable visual-line-mode in compilation buffers
(add-hook! compilation-mode #'visual-line-mode)


(after! go-ts-mode
  (defun +go/go-alt ()
    "Create or go to the corresponding _test.go file for the current Go file."
    (interactive)
    (let* ((current-file (buffer-file-name))
           (current-dir (file-name-directory current-file))
           (current-name (file-name-nondirectory current-file))
           (test-file-name (if (string-match "_test\\.go$" current-name)
                               ;; 如果当前已经是测试文件，则跳转到源文件
                               (replace-regexp-in-string "_test\\.go$" ".go" current-name)
                             ;; 否则跳转到测试文件
                             (replace-regexp-in-string "\\.go$" "_test.go" current-name)))
           (test-file-path (expand-file-name test-file-name current-dir)))

      (if (file-exists-p test-file-path)
          ;; 文件存在，直接打开
          (find-file test-file-path)
        ;; 文件不存在，询问是否创建
        (when (yes-or-no-p (format "Create test file %s? " test-file-name))
          (find-file test-file-path)
          ;; 自动添加基本的测试模板
          (when (zerop (buffer-size))
            (insert (format "package %s\n\nimport (\n\t\"testing\"\n)\n\n"
                            (go-package-name current-dir)))
            (save-buffer)))))))

(after! java-ts-mode
  ;;;###autoload
  (defun +java/copy-java-class-path ()
    "Copy the fully qualified Java class name to clipboard."
    (interactive)
    (unless (or (eq major-mode 'java-mode)
                (eq major-mode 'java-ts-mode))
      (user-error "Not in a Java buffer"))
    (let* ((package (save-excursion
                      (goto-char (point-min))
                      (when (re-search-forward "^package \\([^;]+\\);" nil t)
                        (match-string 1))))
           (class (save-excursion
                    (goto-char (point-min))
                    (when (re-search-forward "^public \\(?:class\\|interface\\|enum\\) \\([A-Za-z0-9_]+\\)" nil t)
                      (match-string 1))))
           (fqn (if (and package class)
                    (concat package "." class)
                  class)))
      (when fqn
        (kill-new fqn)
        (message "Copied: %s" fqn)))))
