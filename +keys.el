;;; +keys.el -*- lexical-binding: t; -*-

;; (with-eval-after-load 'general
;;   ;; 禁用 general-override-mode 以避免与自定义按键绑定冲突
;;   (setq general-override-mode nil)
;;   (setq general-override-auto-enable nil)
;;   )
(add-hook! 'doom-after-init-hook
  (setq doom-leader-alt-key "")
  ;; 批量禁用不需要的 C-x 前缀绑定
  (dolist (key '("+" "*" "#" "$" "'" "\\" "]" "[" "." "f" ";" "^" "`"
                 "B" "C-M-+" "C-M--" "C-M-0" "C-M-=" "z" "X" "w" "n"
                 "C-@" "6" "l" "C-z"))
    (global-unset-key (kbd (concat "C-x " key))))
  ;; 批量禁用不需要的 C-c 前缀绑定
  (dolist (key '("l" "e" "p" "C-b" "i" "M-g"))
    (map! :prefix "C-c " key 'nil))

  (map! :map persp-mode-map "C-c w" nil)
  (map! :map projectile-mode-map "C-c p" nil)

  ;; M-g
  (map! :map  goto-map
        "<TAB>" 'nil
        :desc "list errors" "e" 'flymake-show-buffer-diagnostics
        :desc "lsp doc" "y" 'lsp-ui-doc-glance
        :desc "avy jump char" "c" 'avy-goto-char-2
        :desc "disable" "M-c" 'nil
        :desc "goto file" "f" '+lookup/file
        :desc "goto file" "M-f" #'+lookup/file
        :desc "xdg open at point" "x" #'browse-url
        :desc "imenu" "i" 'imenu)

  (map! :desc "centaur-tabs forward" "M-] M-]" #'centaur-tabs-forward
        :desc "centaur-tabs backward" "M-[ M-[" #'centaur-tabs-backward
        :desc "vc-next-hunk" "M-] g" #'+vc-gutter/next-hunk
        :desc "vc-previous-hunk" "M-[ g" #'+vc-gutter/previous-hunk
        :desc "flymake-goto-next-error" "M-] e" #'flymake-goto-next-error
        :desc "flymake-goto-previous-error" "M-[ e" #'flymake-goto-prev-error
        :desc "end of fun" "M-] f" #'end-of-defun
        :desc "beginning of fun" "M-[ f" #'beginning-of-defun)

  ;; 快速删除一个单词或选中区域
  (define-key global-map (kbd "C-w")
              (lambda ()
                (interactive)
                (if mark-active
                    (kill-region (region-beginning) (region-end))
                  (backward-kill-word 1))))

  (with-eval-after-load 'vterm
    (map! :map vterm-mode-map
          :desc "toggle vterm" "C-c /" #'+vterm/toggle))

  (map! :mode gptel-mode
        :desc "gptel send" "C-c C-c" #'gptel-send
        :desc "gptel explain" "C-c C-e" #'gptel-quick
        :desc "gptel rewrite" "C-c C-r" #'gptel-rewrite
        :desc "gptel menu" "C-c C-m" #'gptel-menu
        :desc "gptel open" "C-c C-o" #'gptel)

  ;; (with-eval-after-load 'vertico
  ;;   (map! :map vertico-map
  ;;         :desc "enter directory" "RET" #'vertico-directory-enter))

  (map! :prefix "C-x"
        "k" 'kill-current-buffer     ;; C-x k 关闭当前buffer
        "=" 'balance-windows         ;; C-x = 平衡窗口大小
        "9" 'doom/window-enlargen    ;; C-x 9 增大当前窗口
        ")" 'sp-unwrap-sexp          ;; C-x ) 删除括号但保留内容
        "(" 'sp/wrap-with-pair       ;; C-x ( 添加括号对
        )

  (map! :mode prog-mode
        :desc "Format" "C-c C-k" #'+format/region-or-buffer)

  (map! "M-?" #'+lookup/references
        "M-." #'+lookup/definition
        "C-M-." #'+lookup/type-definition)

  (with-eval-after-load 'lsp-mode
    (map! :map lsp-mode-map
          "M-a" #'lsp-execute-code-action
          "M-r" #'lsp-rename ;; M-r 重命名符号
          "M-k" #'lsp-ui-doc-glance ;; M-k 查看文档
          "C-c C-k" #'+format/region-or-buffer ;; C-c C-k 格式化代码
          "M-?" #'lsp-find-references ;; M-? 查找引用
          "M-." #'lsp-find-definition ;; M-. 跳转定义
          ;;"M-i" #'lsp-imp ;; 查找实现，有冲突
          "C-M-." #'lsp-find-type-definition
          "<f9>"  #'dape-breakpoint-toggle
          "<f10>" #'dape-next
          "<f11>" #'dape-step-in
          "C-<f11>" #'dape-step-out
          "<f5>"  (lambda () (interactive)
                    (if (dape-active-mode)
                        (dape-continue)
                      (dape)))
          "<S-f5>" #'dape-quit
          "C-<f10>" #'dape-until))
  (with-eval-after-load 'python-ts-mode
    (map! :map python-ts-mode-map
          :desc "uv run main.py" "C-c C-r" (lambda () (interactive) (compile "uv run main.py"))))

  (setq which-key-idle-delay 0.2
        which-key-idle-secondary-delay 0.1)

  (with-eval-after-load 'embark
    (dolist (map '(embark-identifier-map
                   embark-command-map
                   embark-function-map
                   embark-symbol-map
                   embark-variable-map))
      (define-key (symbol-value map) (kbd "i") #'+lookup/implementations)
      (define-key (symbol-value map) (kbd "r") #'+lookup/references)
      (define-key (symbol-value map) (kbd "d") #'+lookup/definition)
      (define-key (symbol-value map) (kbd "D") #'+lookup/type-definition)
      (define-key (symbol-value map) (kbd "k") #'+lookup/documentation)
      )
    (map! :map embark-general-map
          :desc "llm quick explain" "?" #'gptel-quick)
    )

  ;; global key
  (map! "M-w" #'my/kill-ring-save
        "M-SPC" #'mark-current-line
        "C-_" 'nil
        "M-_" 'nil
        "M-;" #'my-comment-dwim
        "M-u" 'smart-upcase-char-or-word
        "M-l" 'smart-downcase-char-or-word
        "C-o" #'pop-global-mark ;; C-o 跳回上一个光标位置

        "C-z" #'undo-fu-only-undo ;; C-z 撤销
        "C-S-z" #'undo-fu-only-redo ;; C-S-z 重做
        "C-." #'repeat ;; C-. 重复执行上一个复杂命令
        "C-r" #'consult-recent-file ;; 最近打开的文件
        "C-s" #'consult-line ;; 搜索当前buffer
        "C-M-s" #'sp/wrap-with-pair ;; 快速添加括号对
        "C--" #'er/contract-region      ;; C-x - 收缩选择区域
        "C-=" #'er/expand-region ;; C-= 扩大选择区域
        "C-u" #'delete-to-beginning-of-line ;; C-u 删除到行首
        "M-s" #'avy-goto-char-2 ;; C-RET 快速跳转到单词
        "M-/" #'dabbrev-completion ;; M-/ 动态补全缓冲区单词
        "C-M-/" #'dabbrev-expand ;; C-M-/ 动态展开缓冲区单词
        "M-k" #'+lookup/documentation ;; M-k 查看文档
        "M-o" #'copilot-complete ;; M-o copilot补全
        "C-S-l" #'duplicate-line ;; C-S-l 复制当前行
        "C->" #'mc/mark-next-like-this
        "C-<" #'mc/mark-previous-like-this
        "C-c C->" #'mc/mark-all-like-this
        "C-SPC" #'smart-mark-or-expand-region
        "M-1" #'+workspace/switch-to-0
        "M-2" #'+workspace/switch-to-1
        "M-3" #'+workspace/switch-to-2
        "M-4" #'+workspace/switch-to-3
        "M-5" #'+workspace/switch-to-4
        )

  (map! "C-x S" #'doom/sudo-save-buffer) ;; C-x S 以管理员权限保存文件

  (map! :map json-mode-map
        :desc "fold toggle" "C-c TAB" #'+fold/toggle
        :desc "beautify json" "C-c C-k" #'json-mode-beautify
        :desc "nullify sexp" "C-c C-f" #'json-nullify-sexp
        :desc "increase number at point" "C-c C-i" #'json-increment-number-at-point)


  (map! :prefix "C-c" ;; C-c 开头的快捷键
        :desc "fold toggle" "<TAB>" #'+fold/toggle
        :desc "find file" "SPC" #'consult-find-file-or-projectile ;; C-c C-c 打开文件
        :desc "dirvish" "d" #'dirvish
        :desc "verterm" "/" #'+vterm/toggle
        :desc "query replace" "%" #'query-replace-regexp
        (:prefix "s"
         :desc "consult ripgrep" "s" #'consult-buffer ;; C-c s s 在项目中搜索文本
         :desc "consult grep" "g" #'consult-ripgrep ;; C-c s g 使用grep搜索文本
         :desc "consult imenu" "i" #'consult-imenu ;; C-c s i 跳转到符号
         :desc "consult line" "l" #'consult-line ;; C-c s l 搜索当前buffer
         :desc "consult multi-occur" "o" #'consult-multi-occur ;; C-c s o 多buffer搜索
         :desc "search and jump file" "f" #'+lookup/file ;; C-c s f 跳转到文件
         )
        (:prefix "m"
         :desc "mark current line" "l" #'mark-current-line
         :desc "consult mark" "m" #'consult-mark ;; C-c m m 跳转到标记
         :desc "consult global mark" "g" #'consult-global-mark ;; C-c m g 全局标记
         :desc "consult bookmark" "b" #'consult-bookmark ;; C-c m b 书签
         )
        (:prefix "f"
         :desc "find file" "f" #'consult-find-file-or-projectile ;; C-c f f 打开文件
         :desc "jump to bookmark" "b" #'consult-bookmark ;; C-c f b 跳转到书签
         :desc "open config file" "c" #'doom/open-private-config ;; C-c f c 打开配置文件
         :desc "open dotfile" "d" #'doom/open-dotfile ;; C-c f d 打开dotfile
         :desc "open org-roam file" "o" #'org-roam-node-find ;; C-c f o 打开org-roam文件
         :desc "delete this file" "D" #'doom/delete-this-file ;; C-c f D 删除当前文件
         :desc "rename this file" "R" #'rename-file ;; C-c f r 重命名当前文件
         )
        (:prefix "b"
         :desc "switch buffer" "b" #'switch-to-buffer ;; C-c b b 切换buffer
         :desc "project buffer" "p" #'projectile-switch-to-buffer
         :desc "new buffer" "n" #'my-new-scratch-buffer ;; 新建一个buffer
         :desc "kill buffer" "k" #'kill-this-buffer ;; C-c b k 关闭当前buffer
         :desc "ibuffers" "i" #'ibuffer ;; C-c b i 列出所有buffer
         :desc "revert buffer" "r" #'revert-buffer ;; C-c b r 刷新当前buffer
         :desc "kill other buffers" "c" #'doom/kill-other-buffers ;; C-c b o 关闭其他buffer
         :desc "open project scratch buffer" "x" #'doom/open-project-scratch-buffer ;; C-c b x 打开scratch buffer
         :desc "open scratch" "o" #'doom/toggle-scratch-buffer
         :desc "rename buffer" "R" #'rename-buffer
         )
        (:prefix "g"
         :desc "magit status" "g" #'magit-status ;; C-c g g 打开magit状态
         :desc "magit dispatch" "d" #'magit-dispatch ;; C-c g d 打开magit调度
         :desc "magit pull" "p" #'magit-pull ;; C-c g p 拉取最新代码
         :desc "magit push" "P" #'magit-push ;; C-c g P 推送代码
         :desc "magit fetch" "f" #'magit-fetch ;; C-c g f 获取远程更新
         :desc "magit fixtup" "F" #'magit-commit-fixup ;; C-c g F 修复提交
         :desc "magit branch" "b" #'magit-branch ;; C-c g b 管理分支
         :desc "magit checkout" "c" #'magit-checkout ;; C-c g c 切换分支
         :desc "magit log" "l" #'magit-log ;; C-c g l 查看提交日志
         :desc "next-hunk" "n" #'+vc-gutter/next-hunk
         :desc "previous-hunk" "p" #'+vc-gutter/previous-hunk
         :desc "stage-hunk" "s" #'+vc-gutter/next-h
         :desc "revert-hunk" "r" #'+vc-gutter/revert-hunk
         )
        (:prefix "p"
         :desc "switch project" "p" #'projectile-switch-project ;; C-c p p 切换项目
         :desc "find files" "f" #'projectile-find-file ;; C-c p f 在项目中查找文件
         :desc "search" "s" #'consult-ripgrep  ;; C-c p s 在项目中搜索文本
         :desc "switch buffer" "b" #'projectile-switch-to-buffer ;; C-c p b 切换项目buffer
         :desc "kill buffers" "k" #'projectile-kill-buffers ;; C-c p k 杀死项目buffer
         :desc "add project" "a" #'projectile-add-known-project ;; C-c p a 添加已知项目
         :desc "remove project" "d" #'projectile-remove-known-project ;; C-c p d 移除已知项目
         :desc "invalidate cache" "i" #'projectile-invalidate-cache ;; C-c p i 使项目缓存失效
         :desc "comple" "c" #'projectile-compile-project ;; C-c p c 编译项目
         :desc "run" "x" #'projectile-run-project ;; C-c p x 运行
         :desc "edit local vars" "e" #'projectile-edit-dir-locals ;; C-c p e 编辑项目本地变量
         :desc "search project files" "SPC" #'projectile-find-file ;; C-c p SPC 在项目文件中搜索
         :desc "index project" "I" #'projectile-index-project ;; C-c p I 索引项目
         :desc "replace" "R" #'projectile-replace) ;; C-c p R 替换文本
        (:prefix "w"
         :desc "split window right" "v" #'split-window-right ;; C-c w s 符合vim习惯
         :desc "save session" "s" #'+workspace/save ;; C-c W s 保存工作区
         :desc "load session" "l" #'+workspace/load ;; C-c W l 加载工作区
         :desc "recover last session" "r" #'+workspace/restore-last-session ;; C-c w r 回复最后一次会话
         :desc "rename workspace" "R" #'+workspace/rename ;; C-c w R 重命名工作区
         :desc "new workspace" "n" #'+workspace/new ;; C-c W n 新建工作区
         :desc "kill workspace" "d" #'+workspace/kill ;; C-c W d 删除工作区
         :desc "display workspaces" "i" #'+workspace/display ;; C-c W i 显示工作区列表
         :desc "next workspace" "w" #'+workspace/switch-to ;; C-c w w 切换到下一个工作区
         )
        (:prefix "a" ;; make menu selection persistent across this Emacs session by pressing C-x C-s:
         :desc "open llm buffer" "RET" #'gptel
         :desc "set org topic" "t" #'gptel-org-set-topic ;; C-c a t 设置org topic
         :desc "send message" "s" #'gptel-send
         :desc "quick explain" "e" #'gptel-quick
         :desc "open menu" "m" #'gptel-menu
         :desc "add file" "f" #'gptel-add-file
         :desc "add comment at point" "c" #'gptel-add-comment
         :desc "add buffer" "b" #'gptel-add
         :desc "rewrite with response" "r" #'gptel-rewrite
         :desc "one shot question" "a" #'gptel-ask-from-minibuffer
         :desc "move to next prompt" "C-e" #'gptel-end-of-response
         :desc "stop response" "S" #'gptel-abort)
        (:prefix "n"
         :desc "browse notes" "b" #'+default/browse-notes ;; C-c n b 浏览笔记
         :desc "agender" "a" #'org-agenda
         :desc "capture" "n" #'org-capture
         :desc "roam buffer toggle" "l" #'org-roam-buffer-toggle
         :desc "roam node find" "f" #'org-roam-node-find
         :desc "roam node insert" "i" #'org-roam-node-insert
         :desc "daily note" "d" #'org-roam-dailies-capture-today
         :desc "todo" "t" #'org-todo-list)
        (:prefix "i"
         :desc "insert emoji" "e" #'emoji-search
         :desc "insert char" "c" #'insert-char
         :desc "insert unicode" "y" #'+default/yank-pop)
        (:prefix "t"
         :desc "toggle flymake" "f" #'flymake-mode
         :desc "toggle display-line-numbers-mode" "l" #'display-line-numbers-mode
         :desc "toggle wrap" "w" #'toggle-truncate-lines
         :desc "toggle indent guide" "i" #'highlight-indent-mode)
        )


  (which-key-add-key-based-replacements
    "C-x RET" "coding system"
    "C-x 4" "open in other window"
    "C-x 5" "open in other frame"
    "C-x 8" "insert special char"
    "C-x 8 e" "insert emoji"
    "C-x >" "view content left"
    "C-x <" "view content right"
    "C-x a" "abbrev"
    "C-x b" "project buffer"
    "C-x x" "font and buffer"
    "C-x r" "registers"
    "C-x K" "kill buffer in all window"
    "C-c p" "projectile"
    "C-c g" "magit"
    "C-c b" "switch buffer"
    "C-c d" "dirvish"
    "C-c w" "workspacape"
    "C-c m" "marks/bookmakrs"
    "C-c a" "llm"
    "C-c s" "search"
    "C-c f" "files"
    "C-c &" "snippets"
    "C-c @" "outlines"
    "C-c n" "notes"
    "C-c i" "insert"
    )
  )

(modulep! :editor evil
          (with-eval-after-load 'evil
            (evil-mode -1)
            (map! :map evil-normal-state-map
                  "C-n" #'next-line ;; C-n 向下
                  "C-p" #'previous-line ;; C-p 向上
                  "C-e" #'end-of-line ;; C-e 行尾
                  "C-f" #'forward-char ;; C-f 向右
                  "C-b" #'backward-char ;; C-b 向左
                  "q" 'nil ;; 禁用q键
                  "Q" #'evil-record-macro ;; Q 录制宏
                  "/" #'consult-line
                  )
            (map! :map evil-insert-state-map
                  "C-n" #'next-line ;; C-n 向下
                  "C-p" #'previous-line ;; C-p 向上
                  "C-e" #'doom/forward-to-last-non-comment-or-eol ;; C-e 行尾
                  "C-d" #'delete-char ;; C-d 删除一个字符
                  "C-k" #'kill-line ;; C-k 删除到行尾
                  "C-_" 'nil
                  "M-_" 'nil
                  "C-c C-k" #'+format/region-or-buffer ;; C-c C-k 格式化代码
                  "C-z" #'undo-fu-only-undo ;; C-z 撤销
                  "C-S-z" #'undo-fu-only-redo ;; C-S-z 重做
                  "C-." #'repeat ;; C-. 重复执行上一个复杂命令
                  n     "C-r" #'consult-recent-file ;; 最近打开的文件
                  "C-s" #'consult-line ;; 搜索当前buffer
                  "C-M-s" #'sp/wrap-with-pair ;; 快速添加括号对
                  "C-c SPC" #'find-file;; 搜索当前目录的文件
                  "C--" #'er/contract-region      ;; C-x - 收缩选择区域
                  "C-u" #'delete-to-beginning-of-line ;; C-u 删除到行首
                  "C-v" #'evil-scroll-page-down ;; C-v 向下翻页
                  "M-vp" #'evil-scroll-page-up ;; M-v 向上
                  "C-'" #'avy-goto-char-2 ;; C-RET 快速跳转到单词
                  )
            (map! :map  corfu-mode-map
                  :i "C-SPC" 'set-mark-command ;; C-SPC 设置标记开始选择
                  )
            )
          )

;; emacs模式技巧
;; M-% replace regexp
;; C-M-h 快速标记整个函数
;; 查看按键绑定 C-h k <key>
;; C-M-<end> 跳转到函数末尾
;; C-M-<home> 跳转到函数开头
;; C-M-f 跳转字符串或者括号的下一个位置 C-M-b 跳转字符串或者括号的上一个位置
;; C-M-d
;; C-M-Backspace 删除括号或双引号
;; C-/ undo
;; C-S-/ redo
;; 删除括号或者双引号里面的内容,可以使用embark  C-; Backspace键 也可以连续使用C-space 选择内容后使用C-w删除
;; 使用C-x M-: 查看复杂的传参的函数调用
;; M-i tab
;; M-? reference
;; M-. definition
;; C-S-z redo
;; C-z undo
;; C-; embark-act ;; 对当前光标下的内容执行embark操作
;; C-' avy-goto-char-2 快速跳转到单词
;; M-: ;; 手动执行一个elisp函数
;; C-a C-k C-z ;; 快速复制一行
;; C-a C-k ;; 快速选择一
;; C-x h ;; 快速选择整个buffer
;; C-space C-= ;; 快速选择单词、一行、段落、整个buffer
;; M-u ;; 大写一个字符
;; M-l ;; 小写一个字符
;; M-g M-g ;; 快速跳转行
;; C-` ;; +popup/toggle 切换弹框buffer
;; C-M 键位适配
;; C-M-n 向下跳转符号
;; (buffer-substring-no-properties (point-min) (point-max))   获取整个缓冲区的内容
;; (save-excursion (mark-defun) (buffer-substring-no-properties (region-beginning) (region-end)) 获取当前函数的内容)
;; (buffer-substring-no-properties (save-excursion (beginning-of-defun) (point)) (save-excursion (end-of-defun) (point)))
