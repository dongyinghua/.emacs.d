;;lexical binding很重要！！！
;;有些package是需要激活lexical binding的
;;Emacs官方文档中关于lexical binding的链接
;;https://www.gnu.org/software/emacs/manual/html_node/elisp/Lexical-Binding.html
;;默认lexical binding是不激活的，需要在文件最开头添加下面语句
;; -*- lexical-binding: t -*-

;;子龙山人的镜像速度快，但是package的版本可能落后于官方源
(require 'package)
(setq package-archives '(("gnu"   . "http://elpa.zilongshanren.com/gnu/")
                          ("melpa" . "http://elpa.zilongshanren.com/melpa/")))
(package-initialize)

;;防止反复调用 package-refresh-contents 会影响加载速度
(when (not package-archive-contents)
  (package-refresh-contents))

;;修改快捷键映射
(setq mac-option-modifier 'meta
  mac-command-modifier 'super)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t))
(eval-when-compile
  (require 'use-package))

;; Keep ~/.emacs.d/ clean.
(use-package no-littering
  :ensure t
  :demand t)

;; Bootstrap `quelpa'.
(use-package quelpa
  :ensure t
  :commands quelpa
  :custom
  (quelpa-git-clone-depth 1)
  (quelpa-self-upgrade-p nil)
  (quelpa-update-melpa-p nil)
  (quelpa-checkout-melpa-p nil))

(let ((dir (locate-user-emacs-file "lisp")))
  (add-to-list 'load-path (file-name-as-directory dir))
  (add-to-list 'load-path (file-name-as-directory (expand-file-name "lang" dir))))

(global-set-key (kbd "s-a") 'mark-whole-buffer) ;;对应mac上面的Command-a 全选
(global-set-key (kbd "s-c") 'kill-ring-save) ;;对应mac上面的Command-c 复制
(global-set-key (kbd "s-s") 'save-buffer) ;; 对应mac上面的Command-s 保存
(global-set-key (kbd "s-v") 'yank) ;;对应mac上面的Command-v 粘贴
(global-set-key (kbd "s-z") 'undo) ;;对应mac上面的Command-z 撤销
(global-set-key (kbd "s-x") 'kill-region) ;;对应mac上面的Command-x 剪切
(global-set-key (kbd "s-q") 'save-buffers-kill-terminal) ;;对应mac上面的Command-q 退出应用

;;;;“C-x C-h”可以看到所有以“C-x”开头的快捷键
;;“C-c”是Emacs给用户保留的快捷键前缀，“C-x”是给系统的
;;尽量全部用“C-c”作为自定义快捷键的前缀，这样既可以用embark来直接在minibuffer中执行命令
;;“C-c C-h”可以查看以“C-c”为前缀的快捷键，然后在embark加持下，就可以选择快捷键来直接执行命令
;;同理“C-x C-h”也可以用这种方式
(global-set-key (kbd "C-c e") 'editorconfig-format-buffer)

;;find-function跳到函数的源文件
(global-set-key (kbd "C-h C-f") 'find-function)

;;快速打开配置文件
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
;;这一行代码，将函数 open-init-file 绑定到 <f1> 键上
(global-set-key (kbd "<f1>") 'open-init-file)

(defun open-init-org()
  (interactive)
  (find-file "~/.emacs.d/lisp/init-org.el"))
(global-set-key (kbd "<f2>") 'open-init-org)

(global-set-key (kbd "C-c C-e b") 'eval-buffer)



;;显示行号
;;(global-linum-mode 1)
;;用linum-mode的话会和viusal-fill-column-mode冲突，导致行号显示不出来而且移动光标会出现行号一闪就消失的情况
(global-display-line-numbers-mode)

;;隐藏开始界面
;;(setq inhibit-startup-screen 1)

;;启动最大化
;;以函数调用的方式写在配置文件中，就可以在启动时执行这些函数
(toggle-frame-maximized)

;;eldoc-mode 显示函数或变量的信息

;;major-mode和minor-mode的区别
;;Major Mode通常是定义对于一种文件类型编辑的核心规则，例如语法高亮、缩进、快捷键绑定等。
;;而 Minor Mode 是除去 Major Mode 所提供的核心功能以外的额外编辑功能（辅助功能）。
;;看一种文件类型的major-mode用快捷键“C-h m”

;;关闭工具栏和右侧滑动
;;正数表示t，非正数表示nil
;;注：不知道为什么最基本的emacs不识别nil，就算用nil赋值，其值依旧是t
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;修改光标样式，“C-h v”查看详细内容
(setq-default cursor-type 'bar)

;;中文与英文字体设置
;;https://www.cnblogs.com/galaxy-gao/p/4445757.html
;;Setting English Font
(set-face-attribute
  'default nil :font "Source Code Pro 20" :weight 'normal)
;;Chinese Font
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
    charset
    (font-spec :family "冬青黑体简体中文 W6" :size 21)))

;;括号匹配
(electric-pair-mode t)
(setq electric-pair-pairs '(
                             (?\{ . ?\})
                             (?\“ . ?\”)
                             (?\< . ?\>)
                             ))

;;删除很多空格使使用的
(global-hungry-delete-mode t)
(setq hungry-delete-join-reluctantly nil)

;;org-mode相关配置
;;(load-file "~/.emacs.d/init-org.el")
(require 'init-org)

;;补全
;;tab键
(setq tab-always-indent 'complete)

;;company
;;放弃使用auto-complete，转用company
;;“C-n”和“C-p”来在补全提示栏中选择补全项
(use-package company
  :init
  (global-company-mode)
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0))

;;vertico、orderless、marginalia、embark、consult和embark-consult的组合
;;可以很好的替代ivy和helm
;;minibuffer的增强
;;增强 minibuffer 补全：vertico 和 Orderless
(vertico-mode t);;所有的minibuffer都适用
(setq completion-styles '(orderless));;模糊查找
;;配置 Marginalia 增强 minubuffer 的 annotation
(marginalia-mode t)
;;minibuffer action 和自适应的 context menu：Embark
(global-set-key (kbd "C-;") 'embark-act)
(setq prefix-help-command 'embark-prefix-help-command);;可以实现不用记快捷键，在minibuffer就能执行命令

;;增强文件内搜索和跳转函数定义：Consult
(global-set-key (kbd "C-s") 'consult-line)
;;找到代码中自定义和函数或者使用的packages（前提是用use-package）
(global-set-key (kbd "C-c i") 'consult-imenu)

;;modeline上显示我的所有的按键和执行的命令
;;(keycast-mode t)

;;unicode-fonts
(unicode-fonts-setup)

;;取消emacs的自动备份
(setq make-backup-files nil)

;;彩虹猫nyan cat
;;(require 'nyan-mode)

;;visual-fill-column-mode
;;https://codeberg.org/joostkremers/visual-fill-column
(global-visual-fill-column-mode)
(setq-default visual-fill-column-center-text t)
(advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)
;;(setq-local fill-column 180)
;;visual-fill-column-extra-text-width可以调节文本在中间时，文本两边距屏幕边缘的距离
(setq-default visual-fill-column-extra-text-width '(8 . 8))

;;代码格式.editorconfig
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;;doom-modeline和doom-themes配置
;;(load-file "~/.emacs.d/init-ui.el")
(require 'init-ui)

;;evil模式
(evil-mode 1)
;;下面的代码可以将 insert state map 中的快捷键清空，使其可以回退（Fallback）到 Emacs State 中，
;;这样我们之前的 Emacs State 里面定义的 C-w 等快捷键就不会被 evil insert minor mode state 所覆盖。
(setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map [escape] 'evil-normal-state)

;;latex
(setq-default TeX-engine 'xetex)

;;历史文件
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-item 10)

;;让鼠标滚动更好用
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;;Emacs 有一个自带的包来高亮括号，那就是 show-paren-mode，但它只会在编辑器的
;;光标处在括号上时才会生效，我们可以使用子龙山人的代码来使光标在括号内时高亮括号。
;;将下面的代码添加到 user-config 中。
(define-advice show-paren-function (:around (fn) fix-show-paren-function)
  "Highlight enclosing parens."
  (cond ((looking-at-p "\\s(") (funcall fn))
    (t (save-excursion
         (ignore-errors (backward-up-list))
         (funcall fn)))))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(doom-modeline-bar-width 2)
  '(package-selected-packages
     '(consult embark marginalia orderless company auctex doom-modeline doom-themes evil editorconfig visual-fill-column hungry-delete nyan-mode zotxt unicode-fonts org-roam-bibtex org-roam-ui helm org-roam keycast)))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  )

(provide 'init)
