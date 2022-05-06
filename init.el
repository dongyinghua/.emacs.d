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

(global-set-key (kbd "s-a") 'mark-whole-buffer) ;;对应mac上面的Command-a 全选
(global-set-key (kbd "s-c") 'kill-ring-save) ;;对应mac上面的Command-c 复制
(global-set-key (kbd "s-s") 'save-buffer) ;; 对应mac上面的Command-s 保存
(global-set-key (kbd "s-v") 'yank) ;;对应mac上面的Command-v 粘贴
(global-set-key (kbd "s-z") 'undo) ;;对应mac上面的Command-z 撤销
(global-set-key (kbd "s-x") 'kill-region) ;;对应mac上面的Command-x 剪切
(global-set-key (kbd "s-q") 'save-buffers-kill-terminal) ;;对应mac上面的Command-q 退出应用

(global-set-key (kbd "C-s") 'helm-occur)

;;显示行号
(global-linum-mode 1)

;;隐藏开始界面
(setq inhibit-startup-screen 1)

;;启动最大化
;;以函数调用的方式写在配置文件中，就可以在启动时执行这些函数
(toggle-frame-maximized)

;;eldoc-mode 显示函数或变量的信息

;;major-mode和minor-mode的区别
;;Major Mode 通常是定义对于一种文件类型编辑的核心规则，例如语法高亮、缩进、快捷键绑定等。
;;而 Minor Mode 是除去 Major Mode 所提供的核心功能以外的额外编辑功能（辅助功能）。 
;;看一种文件类型的major-mode用快捷键“C-h m”

;;关闭工具栏和右侧滑动
;;正数表示t，非正数表示nil
;;注：不知道为什么最基本的emacs不识别nil，就算用nil赋值，其值依旧是t
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;修改光标样式，“C-h v”查看详细内容
(setq-default cursor-type 'bar)

;;;中文与英文字体设置
;;https://www.cnblogs.com/galaxy-gao/p/4445757.html
;; Setting English Font
(set-face-attribute
 'default nil :font "Source Code Pro 20" :weight 'normal)
;; Chinese Font
(dolist (charset '(kana han symbol cjk-misc bopomofo))
(set-fontset-font (frame-parameter nil 'font)
charset
(font-spec :family "冬青黑体简体中文 W6" :size 20)))
;;设置中英文字体等宽
;;http://baohaojun.github.io/perfect-emacs-chinese-font.html
(setq face-font-rescale-alist '(("Source Code Pro" . 1.2) ("冬青黑体简体中文 W6" . 1.2)))

;; 快速打开配置文件
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
;; 这一行代码，将函数 open-init-file 绑定到 <f2> 键上
(global-set-key (kbd "<f1>") 'open-init-file)

;;“C-x C-h”可以看到所有以“C-x”开头的快捷键
;;“C-c”是Emacs给用户保留的快捷键前缀，“C-x”是给系统的

;;括号匹配
(electric-pair-mode t)
(setq electric-pair-pairs '(
                               (?\{ . ?\})
                               (?\“ . ?\”)
			       (?\< . ?\>)
			       ))

;;多个删除空格


;;org-mode相关配置
(load-file "~/.emacs.d/init-org.el")
(defun open-init-org()
  (interactive)
  (find-file "~/.emacs.d/init-org.el"))
(global-set-key (kbd "<f2>") 'open-init-org)

;;补全
(use-package auto-complete
  :config
  (global-auto-complete-mode))
(setq tab-always-indent 'complete)
(icomplete-mode 1)

;;modeline上显示我的所有的按键和执行的命令
(keycast-mode t)

;;unicode-fonts
(unicode-fonts-setup)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(zotxt unicode-fonts org-roam-bibtex org-roam-ui auto-complete helm org-roam keycast)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
