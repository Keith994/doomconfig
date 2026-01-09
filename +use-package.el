;;; +use-package.el -*- lexical-binding: t; -*-

(use-package! screenshot
  :defer t)


(use-package! consult-todo
  :ensure t
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! keycast
  :ensure t
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NAVIGATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq evil-cross-lines t
      evil-split-window-below t
      evil-vsplit-window-right t
      ;; Implicit /g flag on evil ex substitution, because I less often want the
      ;; default behavior.
      evil-ex-substitute-global t)

(after! evil
  (evil-define-text-object evil-inner-buffer (count &optional beg end type)
    (list (point-min) (point-max)))
  (define-key evil-inner-text-objects-map "g" 'evil-inner-buffer))


(after! evil-snipe
  (setq evil-snipe-scope 'buffer
        evil-snipe-repeat-scope 'buffer)
  (push 'prodigy-mode evil-snipe-disabled-modes))


;; This package provides the g~ operator to transform CamelCase words into snake_case. You can customize the binding.
;; Try using g~io
; (use-package! evil-string-inflection :after evil)

(use-package! imenu-list
  :ensure t
  :defer t
  :config
  (set-popup-rules! '(("^\\*Ilist\\*" :side right :size 40 :select t))))


(add-hook! 'better-jumper-post-jump-hook #'recenter)

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
          ("E" ,doom-emacs-dir "Emacs directory")
          ))

  (setq dirvish-hide-details '(dired dirvish dirvish-side)
        dirvish-hide-cursor '(dired dirvish dirvish-side))

  (defun dirvish-copy-file-relative-path (&optional multi-line)
    "Copy filepath of marked files.
If MULTI-LINE, make every path occupy a new line."
    (interactive "P")
    (let* ((files (mapcar (lambda (file)
                            (file-relative-name (file-local-name file)))
                          (dired-get-marked-files)))
           (names (mapconcat #'concat files (if multi-line "\n" " "))))
      (dirvish--kill-and-echo (if multi-line (concat "\n" names) names)))))

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
  :init (all-the-icons-ibuffer-mode 1)
  )

(add-hook! 'process-menu-mode-hook
  (setq-local tabulated-list-format [("Process" 30 t)
                                     ("PID"      7 t)
                                     ("Status"   7 t)
                                     ("Buffer"  15 t)
                                     ("TTY"     12 t)
                                     ("Command"  0 t)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPLETION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (modulep! :completion vertico)
  (setq vertico-posframe-border-width 3)
  (setq vertico-posframe-poshandler #'posframe-poshandler-frame-bottom-center)

  ;; Fix jump issue for vertico, https://github.com/hlissner/doom-emacs/issues/5386
  (dolist (func '(+default/search-project))
    (advice-add func :around #'doom-set-jump-a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TERM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! term
  ;; term-mode imenu index
  (add-hook! 'term-mode-hook (setq-local imenu-generic-expression '(("Prompt" "➜\\(.*\\)" 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq +lsp--optimization-init-p t)
(setq +lsp--default-read-process-output-max  (* 16 1024 1024))
(setq +lsp--default-gcmh-high-cons-threshold (* 4 1024 1024 1024))

;; 禁用 Doom 的激进 GC 设置
(setq
      inhibit-compacting-font-caches t               ; 防止字体缓存被压缩
      read-process-output-max (* 16 1024 1024))       ; 16MB - 增加缓冲区
(setq gc-cons-threshold (* 1 1024 1024 1024)
      gc-cons-percentage 0.7
      doom-gc-cons-threshold (* 1 1024 1024 1024)    
      doom-gc-cons-upper-limit (* 1 1024 1024 1024)  ; 1GB
      doom-gc-cons-percentage 0.6)

;; 优化 gcmh - 减少 GC 频率
(after! gcmh
  (setq gcmh-idle-delay 60                             ; 60秒空闲才GC
        gcmh-auto-idle-delay-factor 3                  ; 减少因子
        gcmh-high-cons-threshold (* 1 1024 1024 1024)  ; 2GB（原来是64MB）
        gcmh-verbose nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MISC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook! compilation-mode #'visual-line-mode)

;; evil-escape 包
(use-package! evil-escape
  :config
  (setq evil-escape-key-sequence "jk")  ; 或 "kj"
  (setq evil-escape-delay 0.2)          ; 延迟时间（秒）
  (evil-escape-mode 1))

(use-package! which-func
  :defer t
  :commands which-function)

(use-package! protobuf-mode
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-evil-initial-state!
  '(comint-mode)
  'insert)

(add-hook! 'comint-mode-hook #'visual-line-mode)

(use-package! aider
  :config
  (setq aider-args '("--model" "deepseek/deepseek-coder"))
  (require 'aider-doom))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JAVA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (set-formatter! 'google-java-format "google-java-format -" :modes '(java-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEBUG & RUN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

  (map! :leader
        (:prefix ("d" . "debug")
         :desc "dape breakpoint toggle" "b" #'+my/dape-breakpoint-toggle
         :desc "dape breakpoint remove all" "B" #'+my/dape-breakpoint-remove-all
         ))

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
    ; 增加java调试支持
    (add-to-list 'dape-configs
             `(:modes (java-ts-mode java-mode)
               :ensure dape-ensure-command
               :fn nil
               :command ,(expand-file-name "/usr/lib/jvm/java-21-openjdk/bin/java")
               :command-args ("-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=*:5005"
                              "-jar"
                              ,(expand-file-name "~/.local/share/nvim/mason/share/java-debug-adapter/com.microsoft.java.debug.plugin.jar"))
               :command-insert-stderr t
               :command-cwd (lambda () (dape-cwd))
               :port 5005
               :type "java"
               :request "attach"
               :name "Java Attach"
               :hostName "localhost"
               :port 5005
               :wait-for-port t))  ; 添加这个很重要
    ))

(after! corfu
        (setq corfu-preselect t)
        (map! :map corfu-mode-map
          :ni "C-n" nil
          :ni "C-p" nil))
