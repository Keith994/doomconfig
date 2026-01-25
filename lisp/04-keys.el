;; 04-keys.el --- Basic Keybindings Configuration -*- lexical-binding: t no-byte-compile: t -*-

;; 设置一个快捷键手动刷新包列表
(global-set-key (kbd "C-c p r") 'package-refresh-contents)

(provide '04-keys)
