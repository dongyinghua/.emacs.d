;;; init-ui.el --- Better lookings and appearances. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Visual (UI) configurations,
;; which are the system configurations or some shorter configurations,
;; for better lookings and appearances.
;;

;;; Code:

(require 'init-custom)

;; Emacs basis configurations:
;; 高亮显示当前行
(add-hook 'after-init-hook
	  #'(lambda ()
	      (global-hl-line-mode 1) ; 高亮显示当前行
	      (setq-default line-spacing 0.15) ; 修改行间距
	      (setq inhibit-startup-screen 1) ; 隐藏开始界面

	      ;; 启动最大化，适配 Emacs 29
	      ;; 以函数调用的方式写在配置文件中，就可以在启动时执行这些函数
	      (toggle-frame-maximized)
	      
	      ;; 关闭工具栏和右侧滑动
	      ;; 正数表示t，非正数表示nil
	      ;; 注：不知道为什么最基本的emacs不识别nil，就算用nil赋值，其值依旧是t
	      (tool-bar-mode -1)
	      (scroll-bar-mode -1)

	      ;; 修改光标样式，“C-h v”查看详细内容
	      ;; 我们需要区分 setq 与 setq-default ：
	      ;; setq 设置当前缓冲区（Buffer）中的变量值，
	      ;; setq-default 设 置的为全局的变量的值（具体内容可以在 StackOverflow 找到）。
	      ;; 下面是一个例子，用于 设置光标样式的方法。
	      ;; https://stackoverflow.com/questions/18172728/
	      ;; the-difference-between-setq-and-setq-default-in-emacs-lisp
	      (setq-default cursor-type 'bar)

	      ;;eldoc-mode 显示函数或变量的信息

	      ;; 虚拟化背景
	      ;; (set-frame-parameter (selected-frame) 'alpha '(95 65))
	      ))

;; 显示行号
;; (global-linum-mode 1)
;; 用linum-mode的话会和viusal-fill-column-mode冲突，导致行号显示不出来而且移动光标会出现行号一闪就消失的情况
;; (use-package display-line-numbers
;;   :ensure nil
;;   :defer t
;;   :hook (doom-modeline-mode . global-display-line-numbers-mode))

;; Display ugly ^L page breaks as tidy horizontal lines
;; 快捷键“C-q C-l”
;; https://www.emacswiki.org/emacs/PageBreaks
(use-package page-break-lines
  :diminish
  :ensure t
  :defer t
  :hook (after-init . global-page-break-lines-mode))

(provide 'init-ui)
;;; init-ui.el ends here
