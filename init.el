;;; init.el --- A Fancy and Fast Emacs Configuration. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; A Fancy and Fast Emacs Configuration.
;;


;;; Code:

;;如果不加这句代码的话，就会报错说找不到lisp中的.el文件
(add-to-list 'load-path "~/.emacs.d/lisp/")

;;(require 'cl-lib)

;; Emacs Basic Settings (Emacs内置功能的配置)
(require 'init-basic)
;; Custom Functions
(require 'init-funcs)

;; Package Management
(require 'init-packages)


(require 'init-completion)

(require 'init-tools)

(require 'init-ui)

;; Key Bindings
(require 'init-keybindings)

(require 'init-org)


;; Font Setting
(require 'init-font)

(require 'init-latex)

;; Load `custom.el'
;; 通过“M-x customize-group”修改的设置会自动加载到下面这个文件中
;; 安装的package也会记录到custom.el中
;; 通过“M-x customize-set-variables”修改的设置会在emacs重启后还原
(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file 'no-error 'no-message)

(provide 'init)

;;===========================================================================

;;===========================================================================

;;使用 M-x shell 来学习命令行操作，可以参考 https://missing.csail.mit.edu/2020/ 来学习，living in Emacs。
