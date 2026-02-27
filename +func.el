;;; +func.el -*- lexical-binding: t; -*-

(defun delete-to-beginning-of-line ()
  "Delete from point to the beginning of the line."
  (interactive)
  (delete-region (point) (line-beginning-position)))

(defun consult-find-file-or-projectile ()
  "在projectile项目中查找文件,否则使用consult-find-file"
  (interactive)
  (if (projectile-project-p)
      (projectile-find-file)
    (consult-find)))

(defun sp/wrap-with-pair ()
  "使用sp-wrap-round等函数添加pair, 读取终端的字符决定使用哪个"
  (interactive)
  (let ((key (read-key "Enter a pair character: ")))
    (cond
     ((memq key '(?\( ?\< ?\[ ?\{ ?\" ?\'))
      (sp-wrap-with-pair (char-to-string key)))
     )))

;;;###autoload
(defun async-shell-command-no-window (command)
  "Requisite Documentation"
  (interactive)
  (let
      ((display-buffer-alist
        (list
         (cons
          "\\*Async Shell Command\\*.*"
          (ons #'display-buffer-no-window nil)))))
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
(defun dirvish-copy-file-relative-path (&optional multi-line)
  "Copy filepath of marked files.
If MULTI-LINE, make every path occupy a new line."
  (interactive "P")
  (let* ((files (mapcar (lambda (file)
                          (file-relative-name (file-local-name file)))
                        (dired-get-marked-files)))
         (names (mapconcat #'concat files (if multi-line "\n" " "))))
    (dirvish--kill-and-echo (if multi-line (concat "\n" names) names))))

(defun +my/smart-close-window-enhanced ()
  "Smart window/buffer management:
   - Multiple windows: close current window, keep buffer
   - Single window: kill buffer, switch to another buffer
   - Prompt to save if buffer modified"
  (interactive)
  (let ((current-buffer (current-buffer))
        (window-count (length (window-list))))
    
    (if (> window-count 1)
        ;; Multiple windows: close window
        (delete-window)
      
      ;; Single window: manage buffer
      (when (buffer-modified-p current-buffer)
        (when (y-or-n-p (format "Save buffer %s? " (buffer-name current-buffer)))
          (save-buffer)))
      
      (kill-buffer current-buffer)
      
      ;; Switch to another buffer or create new one
      (let ((other-buffers (seq-filter (lambda (buf)
                                         (and (not (eq buf current-buffer))
                                              (not (string-prefix-p " " (buffer-name buf)))))
                                       (buffer-list))))
        (if other-buffers
            (switch-to-buffer (car other-buffers))
          (switch-to-buffer (generate-new-buffer "*scratch*")))))))

;;;###autoload
(defun smart-mark-or-expand-region ()
  "智能标记/扩展选区"
  (interactive)
  (if (region-active-p)
      (progn
        ;; 扩展选区
        (if (fboundp 'er/expand-region)
            (er/expand-region 1)
          (message "expand-region 未安装")
          (exchange-point-and-mark))
        ;; 显示扩展后的选区大小
        (message "选区扩展: %d 字符"
                 (abs (- (mark) (point)))))
    ;; 设置新标记
    (set-mark-command nil)
    (message "标记已设置")))

(defun smart-upcase-char-or-word ()
  "智能大写当前字符或单词"
  (interactive)
    (if (use-region-p)
        (upcase-region (region-beginning) (region-end)) ;; 如果有选区，转换选区内的文本为大写
      (upcase-char 1))) ;; 否则，大写当前字符

(defun smart-downcase-char-or-word ()
  "智能小写当前字符或单词"
  (interactive)
    (if (use-region-p)
        (downcase-region (region-beginning) (region-end)) ;; 如果有选区，转换选区内的文本为小写
      (progn
        (downcase-region (point) (progn (forward-char 1) (point)))
        (backward-char 1)))) ;; 否则，小写当前字符
