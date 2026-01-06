;;; 5-prog.el -*- lexical-binding: t; -*-

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

(use-package! format-all
  :commands format-all-mode
  :hook (prog-mode . format-all-mode)
  :config
  (setq-default format-all-formatters
                '(("Go"    (gofumpt))
                  ("Shell" (shfmt "-i" "4" "-ci")))))


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
  :defer t
  :config
  ;; Use claude-3-5-sonnet cause it is best in aider benchmark
  ;; (setq aider-args '("--model" "anthropic/claude-3-5-sonnet-20241022"))
  ;; (setenv "ANTHROPIC_API_KEY" anthropic-api-key)
  ;; Or use chatgpt model since it is most well known
  ;; (setq aider-args '("--model" "o3-mini"))
  ;; (setenv "OPENAI_API_KEY" <your-openai-api-key>)
  ;; Or use gemini v2 model since it is very good and free
  (setq aider-args '("--model" "gemini/gemini-2.0-flash-exp"))
  ;; Or use your personal config file
  ;; (setq aider-args `("--config" ,(expand-file-name "~/.aider.conf.yml")))
  ;; ;;
  )

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

