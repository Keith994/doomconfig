;;; +keys.el -*- lexical-binding: t; -*-

(setq doom-leader-key "SPC"
      doom-localleader-key ","
      doom-leader-alt-key 'nil)
;; 批量禁用不需要的 C-x 前缀绑定
(dolist (key '("+" "*" "#" "$" "'" "\\" "]" "[" "." "f" ";" "^" "`"
               "B" "C-M-+" "C-M--" "C-M-0" "C-M-=" "z" "X" "w" "n"
               "C-@" "6" "l" "C-z"))
  (global-unset-key (kbd (concat "C-x " key))))
;; 批量禁用不需要的 C-c 前缀绑定
(dolist (key '("l" "e" "p" "C-b"))
  (map! :prefix "C-c " key 'nil))

(map! "M-I" #'+lookup/implementations)
(map! "M-?" #'+lookup/references)
(map! "M-." #'+lookup/definition)
(map! "M-p" #'+lookup/documentation)
(map! :mode prog-mode "M-R" #'lsp-rename)
(map! "C-'" #'avy-goto-char-2) ;; C-RET 快速跳转到单词
(map! :map  goto-map
      "<TAB>" 'nil
      :desc "lsp doc" "y" 'lsp-ui-doc
      :desc "avy jump char" "c" 'avy-goto-char-2
      :desc "avy jump char" "M-c" 'avy-goto-char-2
      :desc "imenu" "i" 'imenu)

(map! :desc "centaur-tabs forward" "C-S-i" #'centaur-tabs-forward
      :desc "centaur-tabs backward" "C-S-o" #'centaur-tabs-backward
      :desc "centaur-tabs ace jump" "C-S-j" #'centaur-tabs-ace-jump)

(map! :prefix "M-SPC" ;; M-SPC 开头的快捷键
      :desc "find file" "M-SPC" #'find-file ;; M-SPC M-SPC 打开文件
      :desc "switch buffer" "SPC" #'switch-to-buffer ;; M-SPC SPC 切换buffer
      :desc "switch workspace buffer" "b" #'+vertico/switch-workspace-buffer ;; M-SPC b 切换工作区buffer
      :desc "switch buffer" "B" #'switch-to-buffer
      :desc "dirvish" "d" #'dirvish
      :desc "ibuffer" "i" #'ibuffer
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
       :desc "index project" "I" #'projectile-index-project ;; M-SPC p I 索引项目
       :desc "replace" "R" #'projectile-replace) ;; M-SPC p R 替换文本
      (:prefix "w"
       :desc "new workspace" "n" #'+workspace/new ;; M-SPC W n 新建工作区
       :desc "delete workspace" "d" #'+workspace/delete ;; M-SPC W d 删除工作区
       :desc "switch workspace" "SPC" #'+workspace/switch-to ;; M-SPC W s 切换工作区
       :desc "display workspaces" "i" #'+workspace/display ;; M-SPC W i 显示工作区列表
       :desc "next workspace" "<TAB>" #'+workspace/switch-right ;; M-SPC W <TAB> 切换到下一个工作区
       :desc "next workspace" "w" #'+workspace/switch-right ;; M-SPC w w 切换到下一个工作区
       ))

(global-set-key (kbd "C-.") 'repeat) ;; C-. 重复执行上一个复杂命令
(global-set-key (kbd "C-r") 'consult-recent-file) ;; 最近打开的文件
(global-set-key (kbd "C-s") 'consult-line) ;; 搜索当前buffer
(global-set-key (kbd "C-M-s") 'consult-ripgrep) ;; 搜索当前项目的文件
(global-set-key (kbd "C-c SPC") 'find-file);; 搜索当前目录的文件

(defun delete-to-beginning-of-line ()
  "Delete from point to the beginning of the line."
  (interactive)
  (delete-region (point) (line-beginning-position)))
;; 快速删除到行首
(global-set-key (kbd "C-u") 'delete-to-beginning-of-line)

;; 快速删除一个单词或选中区域
(define-key global-map (kbd "C-w")
            (lambda ()
              (interactive)
              (if mark-active
                  (kill-region (region-beginning) (region-end))
                (backward-kill-word 1))))
;; C-SPC 快速选择单词、行、段落、buffer
(define-key global-map (kbd "C-SPC")
            (lambda ()
              (interactive)
              (if mark-active
                  (er/expand-region 1)
                (set-mark-command nil))))

(defun sp/wrap-with-pair ()
  "使用sp-wrap-round等函数添加pair, 读取终端的字符决定使用哪个"
  (interactive)
  (let ((key (read-key "Enter a pair character: ")))
    (cond
     ((memq key '(?\( ?\< ?\[ ?\{ ?\" ?\'))
      (sp-wrap-with-pair (char-to-string key)))
     (t (message "Unsupported pair character: %c" key)))))
(map! :prefix "C-x"
      "-" 'er/contract-region      ;; C-x - 收缩选择区域
      "k" 'kill-current-buffer     ;; C-x k 关闭当前buffer
      "=" 'balance-windows         ;; C-x = 平衡窗口大小
      "9" 'doom/window-enlargen    ;; C-x 9 增大当前窗口
      ")" 'sp-unwrap-sexp          ;; C-x ) 删除括号但保留内容
      "(" 'sp/wrap-with-pair       ;; C-x ( 添加括号对
      )

(which-key-add-key-based-replacements
  "C-x RET" "coding system"
  "C-x 4" "open in other window"
  "C-x 5" "open in other frame"
  "C-x 8" "insert special char"
  "C-x 8 e" "insert emoji"
  "M-SPC <TAB>" "workspace"
  "C-x >" "view content right"
  "C-x <" "view content left"
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


;; (keymap-unset projectile-mode-map "C-c p" t) ;; 取消C-c p的绑定，避免和projectile冲突
;; (keymap-unset general-override-mode-map "C-c p" t) ;; 取消C-c p的绑定，避免和projectile冲突
;; (keymap-unset doom-leader-map "C-c p" t) ;; 取消C-c p的绑定，避免和projectile冲突
;; (keymap-unset global-map "C-c C-b" t) ;; 取消C-c C-b的绑定

;; 技巧
;; C-M-<end> 跳转到函数末尾
;; 删除括号或者双引号里面的内容,可以使用embark  C-; Backspace键 也可以连续使用C-space 选择内容后使用C-w删除
;; 使用C-x M-: 查看复杂的传参的函数调用
;; M-i tab
;; M-? reference
;; M-. definition
;; C-? redo
;; C-; embark
;; M-: ;; 手动执行一个elisp函数
;; C-a C-k C-/ ;; 快速复制一行
;; C-a C-k ;; 快速选择一行
;; C-x h ;; 快速选择整个buffer
;; C-space C-= ;; 快速选择单词、一行、段落、整个buffer
;; M-u ;; 大写一个字符
;; M-l ;; 小写一个字符
;; M-g M-g ;; 快速跳转行
;; C-` ;; +popup/toggle 切换弹框buffer
;; C-M
