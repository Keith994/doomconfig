;;; +lsp.el -*- lexical-binding: t; -*-
;;
; ;; Use format-all by default
; (setq +format-with-lsp nil)

(setq +lsp-prompt-to-install-server 'quiet)

;; Java
;; current VSCode defaults
(setq lsp-java-vmargs '(
                        "-Declipse.application=org.eclipse.jdt.ls.core.id1"
                        "-Dosgi.bundles.defaultStartLevel=4"
                        "-Declipse.product=org.eclipse.jdt.ls.core.product"
                        "-Dlombok.disableConfig=true"
                        "-XX:+UseParallelGC" 
                        "-XX:GCTimeRatio=4" 
                        "-XX:+ParallelRefProcEnabled"
                        "-XX:+UseStringDeduplication"
                        "-XX:MaxGCPauseMillis=200"
                        "-XX:MetaspaceSize=1G"
                        "-XX:MaxMetaspaceSize=2G"
                        "-XX:+UnlockExperimentalVMOptions"
                        "-XX:G1NewSizePercent=20"
                        "-XX:AdaptiveSizePolicyWeight=90" 
                        "-Dsun.zip.disableMemoryMapping=true" 
                        "-javaagent:/home/keith/.local/share/nvim/mason/share/jdtls/lombok.jar"
                        "--add-modules=ALL-SYSTEM"
                        "--add-opens"
                        "java.base/java.util=ALL-UNNAMED"
                        "--add-opens"
                        "java.base/java.lang=ALL-UNNAMED"
                        "-Xmx4G" 
                        "-Xms100m"))
(after! lsp-java
  ;; eclipse.jdt.ls needs java 17+
  ;; Not sure why brew openjdk cannot be recognized by lsp-java, use linux version instead.
  ;; Install with `sudo yum install java-17-amazon-corretto-devel`
  (dolist (java_path `(
                       "/usr/lib/jvm/java-21-openjdk"
                       ))
    (if (file-directory-p java_path)
        (setq lsp-java-configuration-runtimes `[(:name "JavaSE-21"
                                                 :path ,java_path
                                                 :default t)]
              lsp-java-java-path (concat java_path "/bin/java")))))

(add-hook! prog-mode
  (flymake-mode -1))

;; 全局不使用 flymake
(setq flymake-no-changes-timeout nil) ;; 可选：避免 flymake 计时器

(after! flycheck
  (add-hook! prog-mode #'flycheck-mode)


  (setq flycheck-auto-display-errors-after-checking nil)
  (when (fboundp 'flycheck-popup-tip-mode) (flycheck-popup-tip-mode -1))
  (when (fboundp 'flycheck-pos-tip-mode)   (flycheck-pos-tip-mode -1))
  (when (fboundp 'flycheck-inline-mode)    (flycheck-inline-mode -1))

  ;; A. 只关“自动检查”，完全手动触发（最省性能）
  ;;   - 你要手动跑：M-x flycheck-buffer
  ; (setq flycheck-check-syntax-automatically nil)

  ;; B. 如果你仍想“保存时检查”，就用这一行替换上面那行：
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  )

(after! lsp-mode
  (add-hook! 'lsp-help-mode-hook (visual-line-mode 1))
  (setq lsp-log-io nil
        lsp-file-watch-threshold 4000
        lsp-diagnostics-provider :none
        lsp-headerline-breadcrumb-enable t
        lsp-headerline-breadcrumb-icons-enable nil
        lsp-headerline-breadcrumb-segments '(file symbols)
        lsp-imenu-index-symbol-kinds '(File Module Namespace Package Class Method Enum Interface
                                       Function Variable Constant Struct Event Operator TypeParameter))
  (dolist (dir '("[/\\\\]\\.ccls-cache\\'"
                 "[/\\\\]\\.mypy_cache\\'"
                 "[/\\\\]\\.pytest_cache\\'"
                 "[/\\\\]\\.cache\\'"
                 "[/\\\\]\\.clwb\\'"
                 "[/\\\\]\\.env\\'"
                 "[/\\\\]__pycache__\\'"
                 "[/\\\\]bazel-bin\\'"
                 "[/\\\\]bazel-code\\'"
                 "[/\\\\]bazel-genfiles\\'"
                 "[/\\\\]bazel-out\\'"
                 "[/\\\\]bazel-testlogs\\'"
                 "[/\\\\]third_party\\'"
                 "[/\\\\]third-party\\'"
                 "[/\\\\]buildtools\\'"
                 "[/\\\\]out\\'"
                 "[/\\\\]build\\'"
                 ))
    (push dir lsp-file-watch-ignored-directories)))

(after! lsp-ui
  (setq lsp-ui-doc-enable nil
        lsp-lens-enable nil
        lsp-ui-sideline-enable nil
        lsp-ui-doc-include-signature t
        lsp-ui-doc-max-height 15
        lsp-ui-doc-max-width 100))

;; https://github.com/blahgeek/emacs-lsp-booster?tab=readme-ov-file
(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("M-n" . 'copilot-accept-completion)
              ; ("C-TAB" . 'copilot-accept-completion-by-word)
              ; ("C-<tab>" . 'copilot-accept-completion-by-word)
              ))
