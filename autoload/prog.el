;;; autoload/prog.el -*- lexical-binding: t; -*-

(defun my/realgud-eval-nth-name-forward (n)
  (interactive "p")
  (save-excursion
    (let (name)
      (while (and (> n 0) (< (point) (point-max)))
        (let ((p (point)))
          (if (not (c-forward-name))
              (progn
                (c-forward-token-2)
                (when (= (point) p) (forward-char 1)))
            (setq name (buffer-substring-no-properties p (point)))
            (cl-decf n 1))))
      (when name
        (realgud:cmd-eval name)
        nil))))

(defun my/realgud-eval-nth-name-backward (n)
  (interactive "p")
  (save-excursion
    (let (name)
      (while (and (> n 0) (> (point) (point-min)))
        (let ((p (point)))
          (c-backward-token-2)
          (when (= (point) p) (backward-char 1))
          (setq p (point))
          (when (c-forward-name)
            (setq name (buffer-substring-no-properties p (point)))
            (goto-char p)
            (cl-decf n 1))))
      (when name
        (realgud:cmd-eval name)
        nil))))

(defun my/realgud-eval-region-or-word-at-point ()
  (interactive)
  (when-let*
      ((cmdbuf (realgud-get-cmdbuf))
       (process (get-buffer-process cmdbuf))
       (expr
        (if (evil-visual-state-p)
            (let ((range (evil-visual-range)))
              (buffer-substring-no-properties (evil-range-beginning range)
                                              (evil-range-end range)))
          (word-at-point)
          )))
    (with-current-buffer cmdbuf
      (setq realgud:process-filter-save (process-filter process))
      (set-process-filter process 'realgud:eval-process-output))
    (realgud:cmd-eval expr)
    ))

(defun +my//realtime-elisp-doc-function ()
  (-when-let* ((w (selected-window))
               (s (intern-soft (current-word))))
    (describe-symbol s)
    (select-window w)))

;;;###autoload
(defun +my/realtime-elisp-doc ()
  (interactive)
  (when (eq major-mode 'emacs-lisp-mode)
    (if (advice-function-member-p #'+my//realtime-elisp-doc-function eldoc-documentation-function)
        (remove-function (local 'eldoc-documentation-function) #'+my//realtime-elisp-doc-function)
      (add-function :after-while (local 'eldoc-documentation-function) #'+my//realtime-elisp-doc-function))))

;;;###autoload
(defun +my/realgud-eval-nth-name-forward (n)
  (interactive "p")
  (save-excursion
    (let (name)
      (while (and (> n 0) (< (point) (point-max)))
        (let ((p (point)))
          (if (not (c-forward-name))
              (progn
                (c-forward-token-2)
                (when (= (point) p) (forward-char 1)))
            (setq name (buffer-substring-no-properties p (point)))
            (cl-decf n 1))))
      (when name
        (realgud:cmd-eval name)))))

;;;###autoload
(defun +my/realgud-eval-nth-name-backward (n)
  (interactive "p")
  (save-excursion
    (let (name)
      (while (and (> n 0) (> (point) (point-min)))
        (let ((p (point)))
          (c-backward-token-2)
          (when (= (point) p) (backward-char 1))
          (setq p (point))
          (when (c-forward-name)
            (setq name (buffer-substring-no-properties p (point)))
            (goto-char p)
            (cl-decf n 1))))
      (when name
        (realgud:cmd-eval name)))))

;;;###autoload
(defun async-shell-command-no-window (command)
  "Requisite Documentation"
  (interactive)
  (let
      ((display-buffer-alist
        (list
         (cons
          "\\*Async Shell Command\\*.*"
          (cons #'display-buffer-no-window nil)))))
    (async-shell-command
     command nil nil)))

;;;###autoload
(defadvice async-shell-command-no-window (around auto-confirm compile activate)
  (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest args) t))
            ((symbol-function 'y-or-n-p) (lambda (&rest args) t)))
    ad-do-it))

;;;###autoload
(defun display-which-function ()
  (interactive)
  (message (which-function)))

;;;###autoload
(defun display-which-path ()
  "Display and copy the current file path starting from home directory."
  (interactive)
  (let* ((full-path (buffer-file-name))
         (home-path (expand-file-name "~"))
         (relative-path (if (string-prefix-p home-path full-path)
                            (concat "~" (substring full-path (length home-path)))
                          full-path)))
    (message relative-path)
    (kill-new relative-path)))

;;;###autoload
(defun +cc/copy-lldb-breakpoint-of-current-line ()
  "Copy a pdb like breakpoint on the current line."
  (interactive)
  (kill-new
   (concat "b " (file-name-nondirectory (buffer-file-name))
           " : " (number-to-string (line-number-at-pos)))))

;;;###autoload
(defun +go/copy-go-test-run-cmd ()
  "Run single test at point."
  (interactive)
  (if (string-match "_test\\.go" buffer-file-name)
      (save-excursion
        (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?[[:alnum:]]+)[ ]+\\)?\\(Test[[:alnum:]_]+\\)(.*)")
        (let ((cmd (concat "go test "
                           "./" (file-relative-name (file-name-directory (buffer-file-name)) (doom-project-root))
                           " -test.v " "-run" "='^\\Q" (match-string-no-properties 2) "\\E$'")))
          (message cmd)
          (kill-new cmd)))
    (error "Must be in a _test.go file")))

;;;###autoload
(defun +go/copy-go-test-dlv-cmd ()
  "Copy go test cmd."
  (interactive)
  (if (string-match "_test\\.go" buffer-file-name)
      (save-excursion
        (re-search-backward "^func[ \t]+\\(\\(\\w\\|\\s_\\)+\\)")
        (let ((cmd (concat "dlv test --init=breakpoints.dlv "
                           "./" (file-relative-name (file-name-directory (buffer-file-name)) (doom-project-root))
                           " -- -test.v -test.run "
                           "\"^" (match-string 1) "$\""
                           )))
          (message cmd)
          (kill-new cmd)))
    (error "Must be in a _test.go file")))

;;;###autoload
(defun +go/copy-go-breakpoint ()
  "Copy go breakpoint."
  (interactive)
  (let ((cmd (concat "b " (file-relative-name (buffer-file-name) (doom-project-root))
                     ":" (number-to-string (line-number-at-pos)))))
    (message cmd)
    (kill-new cmd)))

;;;###autoload
(defun +go/insert-go-breakpoint ()
  "Insert go breakpoint to breakpoints.dlv."
  (interactive)
  (let ((breakpoint-file (concat (doom-project-root) "breakpoints.dlv"))
        (cmd (concat "b " (file-relative-name (buffer-file-name) (doom-project-root))
                     ":" (number-to-string (line-number-at-pos)) "\n")))
    (with-temp-buffer
      (unless (file-exists-p breakpoint-file)
        (insert "continue")
        (write-file breakpoint-file))
      (find-file breakpoint-file)
      (goto-char (point-min))
      (insert cmd)
      (save-buffer)
      (kill-buffer))))


;; modified from `dape-breakpoint-save'
;;;###autoload
(defun +go/write-project-breakpoints ()
  "Write dape breakpoints to breakpoints.dlv file in project root."
  (interactive)
  (when (eq major-mode 'go-ts-mode)
    (let ((project-root (doom-project-root))
          (breakpoint-file (concat (doom-project-root) "breakpoints.dlv"))
          (result ""))
      (cl-loop for breakpoint in dape--breakpoints
               for path = (dape--breakpoint-file-name breakpoint)
               for line = (dape--breakpoint-line breakpoint)
               for condition = (dape--breakpoint-value breakpoint)
               when (and path (string-prefix-p project-root path))
               do (setq result (concat result "b "
                                       (file-relative-name path project-root)
                                       ":"
                                       (number-to-string line)
                                       (if condition (concat " " condition) "")
                                       "\n")))
      (with-temp-buffer
        (insert result)
        (insert "continue")
        (write-file breakpoint-file)))))

;;;###autoload
(defun +ai/copy-current-line ()
  "Copy the context of current line."
  (interactive)
  (let ((cmd (concat "Given `" (file-relative-name (buffer-file-name) (doom-project-root))
                     "` line " (number-to-string (line-number-at-pos)) " as context.\n")))
    (message cmd)
    (kill-new cmd)))

;;;###autoload
(defun +ai/copy-current-function ()
  "Copy the context of current function."
  (interactive)
  (let ((cmd (concat "Given `" (file-relative-name (buffer-file-name) (doom-project-root))
                     "` line " (number-to-string
                                (save-excursion (beginning-of-defun) (line-number-at-pos))) " as context.\n")))
    (message cmd)
    (kill-new cmd)))

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
      (message "Copied: %s" fqn))))

;;;###autoload
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
          (save-buffer))))))
