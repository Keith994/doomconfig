;;; +keys.el -*- lexical-binding: t; -*-

(defun delete-to-beginning-of-line ()
  "Delete from point to the beginning of the line."
  (interactive)
  (delete-region (point) (line-beginning-position))
  (delete-char 1 nil))

(defun consult-find-file-or-projectile ()
  "在projectile项目中查找文件,否则使用consult-find-file"
  (interactive)
  (if (projectile-project-p)
      (projectile-find-file)
    (consult-find)))

;; (setq doom-leader-key "SPC"
;;       doom-localleader-key ","
;;      doom-leader-alt-key nil)
;; 批量禁用不需要的 C-x 前缀绑定
(dolist (key '("+" "*" "#" "$" "'" "\\" "]" "[" "." "f" ";" "^" "`"
               "B" "C-M-+" "C-M--" "C-M-0" "C-M-=" "z" "X" "w" "n"
               "C-@" "6" "l" "C-z"))
  (global-unset-key (kbd (concat "C-x " key))))
;; 批量禁用不需要的 C-c 前缀绑定
(dolist (key '("l" "e" "p" "C-b"))
  (map! :prefix "C-c " key 'nil))
(defun sp/wrap-with-pair ()
  "使用sp-wrap-round等函数添加pair, 读取终端的字符决定使用哪个"
  (interactive)
  (let ((key (read-key "Enter a pair character: ")))
    (cond
     ((memq key '(?\( ?\< ?\[ ?\{ ?\" ?\'))
      (sp-wrap-with-pair (char-to-string key)))
     )))

(map! :map  goto-map
      "<TAB>" 'nil
      :desc "list errors" "e" 'flymake-show-buffer-diagnostics
      :desc "lsp doc" "y" 'lsp-ui-doc-glance
      :desc "avy jump char" "c" 'avy-goto-char-2
      :desc "disable" "M-c" 'nil
      :desc "imenu" "i" 'imenu)

(map! :desc "centaur-tabs forward" "M-] M-]" #'centaur-tabs-forward
      :desc "centaur-tabs backward" "M-[ M-[" #'centaur-tabs-backward
      :desc "vc-next-hunk" "M-] g" #'+vc-gutter/next-hunk
      :desc "vc-previous-hunk" "M-[ g" #'+vc-gutter/previous-hunk
      :desc "flymake-goto-next-error" "M-] e" #'flymake-goto-next-error
      :desc "flymake-goto-previous-error" "M-[ e" #'flymake-goto-prev-error)

(map! :prefix "M-SPC" ;; M-SPC 开头的快捷键
      :desc "find file" "M-SPC" #'consult-find-file-or-projectile ;; M-SPC M-SPC 打开文件
      :desc "switch buffer" "SPC" #'switch-to-buffer ;; M-SPC SPC 切换buffer
      :desc "switch buffer" "B" #'switch-to-buffer
      :desc "dirvish" "d" #'dirvish
      :desc "ibuffer" "i" #'ibuffer
      (:prefix "s"
       :desc "consult ripgrep" "s" #'consult-buffer ;; M-SPC s s 在项目中搜索文本
       :desc "consult grep" "g" #'consult-grep ;; M-SPC s g 使用grep搜索文本
       :desc "consult imenu" "i" #'consult-imenu ;; M-SPC s i 跳转到符号
       :desc "consult line" "l" #'consult-line ;; M-SPC s l 搜索当前buffer
       :desc "consult multi-occur" "o" #'consult-multi-occur ;; M-SPC s o 多buffer搜索
       )
      (:prefix "m"
       :desc "consult mark" "m" #'consult-mark ;; M-SPC m m 跳转到标记
       :desc "consult global mark" "g" #'consult-global-mark ;; M-SPC m g 全局标记
       :desc "consult bookmark" "b" #'consult-bookmark ;; M-SPC m b 书签
       )
      (:prefix "f"
       :desc "find file" "f" #'consult-find-file-or-projectile ;; M-SPC f f 打开文件
       :desc "jump to bookmark" "b" #'consult-bookmark ;; M-SPC f b 跳转到书签
       :desc "open config file" "c" #'doom/open-private-config ;; M-SPC f c 打开配置文件
       :desc "open dotfile" "d" #'doom/open-dotfile ;; M-SPC f d 打开dotfile
       :desc "open org-roam file" "o" #'org-roam-node-find ;; M-SPC f o 打开org-roam文件
       )
      (:prefix "b"
       :desc "switch buffer" "b" #'switch-to-buffer ;; M-SPC b b 切换buffer
       :desc "previous buffer" "p" #'previous-buffer ;; M-SPC b p 上一个buffer
       :desc "next buffer" "n" #'next-buffer ;; M-SPC b n 下一个buffer
       :desc "kill buffer" "k" #'kill-this-buffer ;; M-SPC b k 关闭当前buffer
       :desc "ibuffers" "i" #'ibuffer ;; M-SPC b i 列出所有buffer
       :desc "revert buffer" "r" #'revert-buffer ;; M-SPC b r 刷新当前buffer
       :desc "kill other buffers" "c" #'doom/kill-other-buffers ;; M-SPC b o 关闭其他buffer
       )
      (:prefix "g"
       :desc "magit status" "g" #'magit-status ;; M-SPC g s 打开magit状态
       :desc "magit dispatch" "d" #'magit-dispatch ;; M-SPC g d 打开magit调度
       :desc "magit pull" "p" #'magit-pull ;; M-SPC g p 拉取最新代码
       :desc "magit push" "P" #'magit-push ;; M-SPC g P 推送代码
       :desc "magit fetch" "f" #'magit-fetch ;; M-SPC g f 获取远程更新
       :desc "magit branch" "b" #'magit-branch ;; M-SPC g b 管理分支
       :desc "magit checkout" "c" #'magit-checkout ;; M-SPC g c 切换分支
       :desc "magit log" "l" #'magit-log ;; M-SPC g l 查看提交日志
       )
      (:prefix "p"
       :desc "switch project" "p" #'projectile-switch-project ;; M-SPC p p 切换项目
       :desc "find files" "f" #'projectile-find-file ;; M-SPC p f 在项目中查找文件
       :desc "search" "s" #'consult-ripgrep  ;; M-SPC p s 在项目中搜索文本
       :desc "switch buffer" "b" #'projectile-switch-to-buffer ;; M-SPC p b 切换项目buffer
       :desc "kill buffers" "k" #'projectile-kill-buffers ;; M-SPC p k 杀死项目buffer
       :desc "add project" "a" #'projectile-add-known-project ;; M-SPC p a 添加已知项目
       :desc "remove project" "d" #'projectile-remove-known-project ;; M-SPC p d 移除已知项目
       :desc "invalidate cache" "i" #'projectile-invalidate-cache ;; M-SPC p i 使项目缓存失效
       :desc "comple" "c" #'projectile-compile-project ;; M-SPC p c 编译项目
       :desc "run" "x" #'projectile-run-project ;; M-SPC p x 运行
       :desc "edit local vars" "e" #'projectile-edit-dir-locals ;; M-SPC p e 编辑项目本地变量
       :desc "search project files" "SPC" #'projectile-find-file ;; M-SPC p SPC 在项目文件中搜索
       :desc "index project" "I" #'projectile-index-project ;; M-SPC p I 索引项目
       :desc "replace" "R" #'projectile-replace) ;; M-SPC p R 替换文本
      (:prefix "w"
       :desc "split window right" "v" #'split-window-right ;; M-SPC w s 符合vim习惯
       :desc "split window below" "s" #'split-window-below ;; M-SPC w v 符合vim习惯
       :desc "new workspace" "n" #'+workspace/new ;; M-SPC W n 新建工作区
       :desc "kill workspace" "d" #'+workspace/kill ;; M-SPC W d 删除工作区
       :desc "display workspaces" "i" #'+workspace/display ;; M-SPC W i 显示工作区列表
       :desc "next workspace" "w" #'+workspace/switch-to ;; M-SPC w w 切换到下一个工作区
       ))


;; 快速删除一个单词或选中区域
(define-key global-map (kbd "C-w")
            (lambda ()
              (interactive)
              (if mark-active
                  (kill-region (region-beginning) (region-end))
                (backward-kill-word 1))))

(map! :prefix "C-x"
      "k" 'kill-current-buffer     ;; C-x k 关闭当前buffer
      "=" 'balance-windows         ;; C-x = 平衡窗口大小
      "9" 'doom/window-enlargen    ;; C-x 9 增大当前窗口
      ")" 'sp-unwrap-sexp          ;; C-x ) 删除括号但保留内容
      "(" 'sp/wrap-with-pair       ;; C-x ( 添加括号对

      )

(after! lsp-mode
  (map! :map lsp-mode-map
        "M-r" #'lsp-rename ;; M-r 重命名符号
        "M-k" #'lsp-ui-doc-glance ;; M-k 查看文档
        "M-?" #'lsp-find-references ;; M-? 查找引用
        "M-." #'lsp-find-definition ;; M-. 跳转定义
        "<f9>"  #'dape-breakpoint-toggle
        "<f10>" #'dape-next
        "<f11>" #'dape-step-in
        "C-<f11>" #'dape-step-out
        "<f5>"  (lambda () (interactive)
                  (if (dape-active-mode)
                      (dape-continue)
                    (dape)))
        "<S-f5>" #'dape-quit
        "C-<f10>" #'dape-until  ))

(setq which-key-idle-delay 0.2
      which-key-idle-secondary-delay 0.1)
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
  "M-SPC p" "projectile"
  "M-SPC g" "magit"
  "M-SPC b" "switch buffer"
  "M-SPC d" "dirvish"
  "M-SPC w" "workspace"
  )

(map! "C-_" 'nil
      "M-_" 'nil
      "C-c C-k" #'+format/region-or-buffer ;; C-c C-k 格式化代码
      "C-z" #'undo-fu-only-undo ;; C-z 撤销
      "C-S-z" #'undo-fu-only-redo ;; C-S-z 重做
      "C-." #'repeat ;; C-. 重复执行上一个复杂命令
      "C-r" #'consult-recent-file ;; 最近打开的文件
      "C-s" #'consult-line ;; 搜索当前buffer
      "C-M-s" #'sp/wrap-with-pair ;; 快速添加括号对
      "C-c SPC" #'find-file;; 搜索当前目录的文件
      "C--" #'er/contract-region      ;; C-x - 收缩选择区域
      "C-=" #'er/expand-region ;; C-= 扩大选择区域
      "C-u" #'delete-to-beginning-of-line ;; C-u 删除到行首
      "C-'" #'avy-goto-char-2 ;; C-RET 快速跳转到单词
      )

(after! evil
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
        "C-e" #'end-of-line ;; C-e 行尾
        "C-d" #'delete-char ;; C-d 删除一个字符
        "C-k" #'kill-line ;; C-k 删除到行尾
        "C-_" 'nil
        "M-_" 'nil
        "C-c C-k" #'+format/region-or-buffer ;; C-c C-k 格式化代码
        "C-z" #'undo-fu-only-undo ;; C-z 撤销
        "C-S-z" #'undo-fu-only-redo ;; C-S-z 重做
        "C-." #'repeat ;; C-. 重复执行上一个复杂命令
        "C-r" #'consult-recent-file ;; 最近打开的文件
        "C-s" #'consult-line ;; 搜索当前buffer
        "C-M-s" #'sp/wrap-with-pair ;; 快速添加括号对
        "C-c SPC" #'find-file;; 搜索当前目录的文件
        "C--" #'er/contract-region      ;; C-x - 收缩选择区域
        "C-u" #'delete-to-beginning-of-line ;; C-u 删除到行首
        "C-v" #'evil-scroll-page-down ;; C-v 向下翻页
        "M-v" #'evil-scroll-page-up ;; M-v 向上
        "C-'" #'avy-goto-char-2 ;; C-RET 快速跳转到单词
        )
  (map! :map  corfu-mode-map
        :i "C-SPC" 'set-mark-command ;; C-SPC 设置标记开始选择
        )
  )
;; emacs模式技巧
;; 查看按键绑定 C-h k <key>
;; C-M-<end> 跳转到函数末尾 C-M-<home> 跳转到函数开头
;; C-M-f 跳转字符串或者括号的下一个位置 C-M-b 跳转字符串或者括号的上一个位置
;; C-M-d
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
