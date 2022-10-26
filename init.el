;;; init.el --- A Fancy and Fast Emacs Configuration. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This file bootstraps the configuration, which is divided into
;; a number of other files.
;;


;;; Code:



;;如果不加这句代码的话，就会报错说找不到lisp中的.el文件
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))


;; ;; Refer to https://github.com/purcell/emacs.d
(require 'init-benchmarking)

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))



;; Load `custom.el'
;; 通过“M-x customize-group”修改的设置会自动加载到下面这个文件中
;; 安装的package也会记录到custom.el中
;; 通过“M-x customize-set-variables”修改的设置会在emacs重启后还原
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'no-error 'no-message)

;;(require 'cl-lib)

;; Package Management
(require 'init-packages)
(require 'init-exec-path) ; Set up $PATH. If you startup emacs in terminal, this is not necessary.

;; Emacs Basic Settings (Emacs内置功能的配置)
(require 'init-basic)
(require 'init-hydra)

;; Font Setting
(require 'init-font)
(require 'init-clipboard)

(require 'init-ui)
(require 'init-dashboard)
(require 'init-persp)

(require 'init-evil)

;; Key Bindings
(require 'init-keybindings)

(require 'init-completion)
(require 'init-tools)
(require 'init-projectile)
(require 'init-git)
(require 'init-treemacs)

(require 'init-compile)
(require 'init-org)

(require 'init-yasnippet)
(require 'init-lsp)
(require 'init-lsp-bridge)

;;Degug
(require 'init-dap)
;;Language
(require 'init-flycheck)
(require 'init-c)
(require 'init-python)

(require 'init-latex)
(require 'init-markdown)

(provide 'init)
;;; init.el ends here
