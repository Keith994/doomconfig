;; init-ui.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;; Optimization
(setq idle-update-delay 0.5)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Initial frame
(setq initial-frame-alist '((top . 0.5)
                            (left . 0.5)
                            (width . 0.7)
                            (height . 0.85)
                            (fullscreen)))

(use-package doom-themes
  :ensure t
  :init (setq doom-theme 'doom-one)
  :config
  ;; 设置主题
  (setq doom-themes-enable-bold t    ; 启用粗体
        doom-themes-enable-italic t) ; 启用斜体
  
  ;; 启用特殊效果
  (doom-themes-visual-bell-config)   ; 视觉提示
  (doom-themes-org-config))          ; Org mode 优化

;; Show line numbers
(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode
          conf-mode toml-ts-mode
          yaml-mode yaml-ts-mode)
         . display-line-numbers-mode)
  :init (setq display-line-numbers-width-start t))

;; Suppress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-scratch-message nil)
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))


(use-package doom-modeline
  :init
  (doom-modeline-mode 1)  ; 启用模式
  :custom
  ;; 可选的自定义配置
  (doom-modeline-window-width-limit (- fill-column 10)) ; 窗口宽度限制
  (doom-modeline-height 25)      ; 设置高度
  (doom-modeline-vcs-max-length 80) ; 版本控制信息最大长度
  (doom-modeline-bar-width 5)    ; 设置 bar 宽度
  (doom-modeline-buffer-file-name-style 'truncate-with-project)  ; 文件名显示方式
  (doom-modeline-icon t)         ; 显示图标（需要安装 all-the-icons）
  (doom-modeline-major-mode-icon t)  ; 显示主模式图标
  (doom-modeline-minor-modes nil)  ; 不显示次要模式
  )

;; Show number of matches in mode-line while searching
(use-package anzu
  :diminish
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :hook (after-init . global-anzu-mode))

(use-package all-the-icons)  ; 只在图形界面安装

(provide '03-ui)
