;;; +base.el -*- lexical-binding: t; -*-

;; User information
(setq user-full-name "Keith Woo"
      user-mail-address "keithwoo1994@163.com")

;; org directory
(setq org-directory "~/org/")

;; Scratch buffer settings
(setq doom-scratch-buffer-major-mode 'emacs-lisp-mode
      confirm-kill-emacs nil)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers t)
(setq display-line-numbers-type 'relative)

;; Manually edit .local/custom.el will break doom updates
(when (file-directory-p custom-file)
  (message (concat "Please delete " custom-file ". And customization in config.el and ui.el.")))

;; Default settings
(setq-default fill-column 120
              delete-trailing-lines t
              vterm-shell "/bin/fish"
              explicit-shell-file-name "/bin/fish")

;; Delete the selection when pasting
(delete-selection-mode 1)

;; disable risky local variables warning
(advice-add 'risky-local-variable-p :override #'ignore)

;; Global Auto-Revert Mode is a global minor mode that reverts any
;; buffer associated with a file when the file changes on disk
(global-auto-revert-mode t)

;; Suppress specific warnings
(custom-set-variables
  '(warning-suppress-log-types '((lsp-mode) (iedit))) ; Suppress lsp-mode and iedit warnings in the log
  '(warning-suppress-types '((iedit)))                ; Suppress iedit warnings
  '(warning-suppress-log-types '((lsp-mode) (iedit))) ; Suppress lsp-mode and iedit warnings in the log
  '(warning-suppress-types '((iedit))))               ; Suppress iedit warnings

;; delete to trash
(setq delete-by-moving-to-trash t)

;; doom modeline settings
(setq doom-modeline-vcs-max-length 80
      doom-modeline-buffer-file-name-style 'relative-from-project
      doom-modeline-icon t
      doom-modeline-major-mode-icon t
      doom-modeline-enable-word-count t)

;; 使用 display-buffer-alist（最标准的方式）
(after! aider
  (add-to-list 'display-buffer-alist
              '((major-mode . #'aider-comint-mode)
                (display-buffer-in-side-window)
                (side . right)
                (slot . 0)
                (window-width . 0.4)))
)

(set-popup-rules! '(("^\\*helpful" :size 0.35)
                    ("^\\*Ibuffer\\*$" :size 0.35)
                    ("^\\*info.*" :size 80 :side right)
                    ("^\\*Man.*" :size 80 :side right)
                    ("^\\*keycast.*" :size 50 :side right)
                    ("^\\*Customize" :actions display-buffer)
                    ("^\\*edit-indirect" :size 0.6)
                    ("^\\*YASnippet Tables\\*$" :size 0.35)
                    ("^\\*grep\\*$" :size 0.35)
                    ("^\\*pytest\\*" :size 0.35)
                    ("^\\*Anaconda\\*$" :size 0.35)
                    ("^\\*aider.*$" :ignore t)
                    ("\\*Async Shell Command\\*$" :side bottom :size 0.30 :select t)
                    ("\\*.*server log\\*$" :side top :size 0.20 :select nil)
                    ((lambda (buf _) (with-current-buffer buf (eq major-mode 'forge-topic-mode))) :size 0.35)
                    ))

;; magit-todos uses hl-todo-keywords
(custom-theme-set-faces! doom-theme
  `(hl-todo :foreground ,(doom-color 'bg)))
(after! hl-todo
  (setq hl-todo-color-background t
        hl-todo-keyword-faces
        `(("TODO"  . ,(doom-color 'orange))
          ("HACK"  . ,(doom-color 'orange))
          ("TEMP"  . ,(doom-color 'orange))
          ("DONE"  . ,(doom-color 'green))
          ("NOTE"  . ,(doom-color 'green))
          ("DONT"  . ,(doom-color 'red))
          ("DEBUG"  . ,(doom-color 'red))
          ("FAIL"  . ,(doom-color 'red))
          ("FIXME" . ,(doom-color 'red))
          ("XXX"   . ,(doom-color 'blue))
          ("XXXX"  . ,(doom-color 'blue)))))

;; 使用 wl-clipboard 实现 Wayland 剪贴板功能
(when (and (eq system-type 'gnu/linux)
           (getenv "WAYLAND_DISPLAY")  ; 确保在 Wayland 环境
           (executable-find "wl-copy")) ; 检查 wl-clipboard 是否存在
  (defun wl-copy-selection (str)
    "Copy STR to Wayland clipboard using wl-copy."
    (with-temp-buffer
      (insert str)
      (call-process-region (point-min) (point-max) "wl-copy" t nil t)))

  (defun wl-paste-selection ()
    "Paste from Wayland clipboard using wl-paste."
    (let ((default-directory "~"))
      (string-trim (shell-command-to-string "wl-paste --no-newline"))))

  ;; 确保 Emacs 正在使用这些自定义函数
  (setq interprogram-cut-function 'wl-copy-selection
        interprogram-paste-function 'wl-paste-selection)

  ;; 启用 Emacs 内的剪贴板功能
  (message "Configured Wayland clipboard support via wl-clipboard."))
