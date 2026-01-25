;; 01-package.el --- Package Management Configuration	-*- lexical-binding: t no-byte-compile: t -*-

;;
;; Configure Load Path
;;

(require 'package)
; (setq package-archives '(("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;                          ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
;                          ("org"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")))
(package-initialize)

;; Load `custom-file'
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; Load `custom-file'
(load custom-file 'noerror)

;; HACK: DO NOT save `package-selected-packages' to `custom-file'
;; @see https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
(defun my-package--save-selected-packages (&optional value)
  "Set `package-selected-packages' to VALUE but don't save to option `custom-file'."
  (when value
    (setq package-selected-packages value))
  (unless after-init-time
    (add-hook 'after-init-hook #'my-package--save-selected-packages)))
(advice-add 'package--save-selected-packages :override #'my-package--save-selected-packages)

;;use-package-always-defer nil
;; Should set before loading `use-package'
(setq use-package-always-ensure t
      use-package-expand-minimally t
      use-package-enable-imenu-support t)

;; Required by `use-package'
(use-package diminish)

;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update)

(provide '01-package)
