;;; +completion.el -*- lexical-binding: t; -*-

;; Corfu Completion
(after! corfu
  (setq +corfu-buffer-scanning-size-limit (* 1 1024 1024)) ; 1 MB
  (setq corfu-preselect t)
  (map! :map corfu-mode-map
        :ni "C-n" nil
        :ni "C-p" nil))
