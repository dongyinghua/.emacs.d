;; -*- lexical-binding: t -*-
;;init-better-defaults.el # 增强内置功能

;;简化Emacs需要用户确认命令是否执行时的“yes or no”
(fset 'yes-or-no-p 'y-or-n-p)

;;括号匹配
(electric-pair-mode t)
(setq electric-pair-pairs '(
                             (?\{ . ?\})
                             (?\“ . ?\”)
                             (?\< . ?\>)
                             ))

;;Emacs 有一个自带的包来高亮括号，那就是 show-paren-mode，但它只会在编辑器的
;;光标处在括号上时才会生效，我们可以使用子龙山人的代码来使光标在括号内时高亮括号。
(define-advice show-paren-function (:around (fn) fix-show-paren-function)
  "Highlight enclosing parens."
  (cond ((looking-at-p "\\s(") (funcall fn))
    (t (save-excursion
         (ignore-errors (backward-up-list))
         (funcall fn)))))

;;删除很多空格使使用的
(global-hungry-delete-mode t)
(setq hungry-delete-join-reluctantly nil)

;;tab键补全
(setq tab-always-indent 'complete)

;;modeline上显示我的所有的按键和执行的命令
;;(keycast-mode t)

;;取消emacs的自动备份
(setq make-backup-files nil)

;;minibuffer模糊查找
(setq completion-styles '(orderless))

;;Emacs Server
;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs)
  )

;;历史文件
(use-package recentf
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-item 10))

;;让鼠标滚动更好用
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;;使用下面的配置文件将删除功能配置成与其他图形界面的编辑器相同，即当你选中一段文字之后输入一个字符会替换掉你选中部分的文字。
(delete-selection-mode t)

(provide 'init-better-defaults)
