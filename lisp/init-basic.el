;; -*- lexical-binding: t -*-
;;init-basic.el 增强Emacs内置功能

(server-start)

;;括号匹配
(electric-pair-mode t)
(setq electric-pair-pairs '(
                             (?\{ . ?\})
                             (?\“ . ?\”)
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

;;modeline上显示我的所有的按键和执行的命令
;;(keycast-mode t)

;;取消emacs的自动备份
(setq make-backup-files nil)

;;minibuffer模糊查找
(setq completion-styles '(orderless))

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

;; 下面的代码可以使 Emacs 自动加载外部修改过的文件。
(global-auto-revert-mode 1)

;; 也许你并不喜欢听到错误时的“哔哔”的警告提示音，使用下面的代码你可以关闭 Emacs 中的警告音
(setq ring-bell-function 'ignore)

;;简化Emacs需要用户确认命令是否执行时的“yes or no”
(fset 'yes-or-no-p 'y-or-n-p)

;; savehist-mode记住之前使用Emacs的工作状态（例如使用M-x的一些命令）
;; 记录到history文件中
;; 因为设置了use-package的ensure的默认值是t，所以在使用use-package对Emacs内置package进行配置时要加上“：ensure: t”
(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
          history-length 1000
          savehist-additional-variables '(mark-ring
                                           global-mark-ring
                                           search-ring
                                           regexp-search-ring
                                           extended-command-history)
          savehist-autosave-interval 300)
  )

;; 记住光标位置
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode)) ;; 这句的意思是在Emacs所有配置文件加载完成后（after-init）再加载save-place-mode

(use-package simple
  :ensure nil
  :hook (after-init . size-indication-mode)
  :init
  (progn
    (setq column-number-mode t)
    ))

(provide 'init-basic)
