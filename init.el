;;lexical binding很重要！！！
;;有些package是需要激活lexical binding的
;;Emacs官方文档中关于lexical binding的链接
;;https://www.gnu.org/software/emacs/manual/html_node/elisp/Lexical-Binding.html
;;默认lexical binding是不激活的，需要在文件最开头添加下面语句
;; -*- lexical-binding: t -*-

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

;;通过“M-x customize-group”修改的设置会自动加载到下面这个文件中
;;通过“M-x customize-set-variables”修改的设置会在emacs重启后还原
(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file 'no-error 'no-message)

(provide 'init)

;;===========================================================================
;;quote是'（单引号）的完全体
;;下面两行的效果完全相同的
;;(quote foo)
;;'foo
;;quote 的意思是不要执行后面的内容，返回它原本的内容，也就是把单引号后面的括号里面的内容当作一个整体。（具体请参考下面的例子）
;;(print '(+ 1 1)) ;; -> (+ 1 1)
;;(print (+ 1 1))  ;; -> 2

;;===========================================================================

;;使用 M-x shell 来学习命令行操作，可以参考 https://missing.csail.mit.edu/2020/ 来学习，living in Emacs。
