;;; +lsp.el -*- lexical-binding: t; -*-

(setq +lsp-prompt-to-install-server 'quiet)

(setq lsp-java-vmargs '("-Declipse.application=org.eclipse.jdt.ls.core.id1"
                        "-Dosgi.bundles.defaultStartLevel=4"
                        "-Declipse.product=org.eclipse.jdt.ls.core.product"
                        "-Dlombok.disableConfig=true"
                        "-XX:+UseG1GC"
                        "-XX:+UseStringDeduplication"
                        "-XX:MaxGCPauseMillis=200"
                        "-XX:MetaspaceSize=512M"
                        "-XX:MaxMetaspaceSize=1G"
                        "-javaagent:/home/keith/.local/share/nvim/mason/share/jdtls/lombok.jar"
                        "--add-modules=ALL-SYSTEM"
                        "--add-opens" "java.base/java.util=ALL-UNNAMED"
                        "--add-opens" "java.base/java.lang=ALL-UNNAMED"
                        "-Xmx4G"
                        "-Xms256m"))
;; eclipse.jdt.ls needs java 17+
;; Not sure why brew openjdk cannot be recognized by lsp-java, use linux version instead.
;; Install with `sudo yum install java-17-amazon-corretto-devel`
(setq lsp-java-configuration-runtimes `[(:name "JavaSE-21"
                                         :path "/usr/lib/jvm/java-21-openjdk"
                                         :default t)])
(setq lsp-java-java-path "/usr/lib/jvm/java-21-openjdk/bin/java")

(add-hook! java-ts-mode
  (lsp))

(after! lsp-mode
  (add-hook! 'lsp-help-mode-hook (visual-line-mode 1))
  (setq lsp-log-io t
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
  (setq lsp-ui-doc-enable t
        lsp-lens-enable nil
        lsp-ui-sideline-enable nil
        lsp-ui-doc-include-signature t
        lsp-ui-doc-max-height 15
        lsp-ui-doc-max-width 100))

;; ============================================================================
;; AI & Coding Assistance
;; accept completion from copilot and fallback to company
;; ===========================================================================
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("M-n" . 'copilot-previous-completion)
              ("M-p" . 'copilot-next-completion)
              ("M-k" . 'copilot-accept-completion)
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion))
  :config
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2)))
