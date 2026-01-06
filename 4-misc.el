;;; 4-misc.el -*- lexical-binding: t; -*-

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
