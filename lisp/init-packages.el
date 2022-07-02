;; -*- lexical-binding: t -*-
;;子龙山人的镜像速度快，但是package的版本可能落后于官方源

(require 'init-funcs)



;;ustc（中科大）的镜像
(setq package-archives '(("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "http://mirrors.ustc.edu.cn/elpa/nongnu/")))

;; 子龙山人（emacs-china）的镜像
;;(setq package-archives '(("gnu"   . "http://1.15.88.122/gnu/")
;;                          ("melpa" . "http://1.15.88.122/melpa/")
;;                          ("nongnu" . "http://1.15.88.122/nongnu/")))

;;防止反复调用 package-refresh-contents 会影响加载速度
(when (not package-archive-contents)
  (package-refresh-contents))

;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;;; 这个配置一定要配置在 use-package 的初始化之前，否则无法正常安装
(assq-delete-all 'org package--builtins)
(assq-delete-all 'org package--builtin-versions)

;; Setup `use-package'
;; (package-installed-p 'use-package) 如果没有安装use-package，就返回nil
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; 以后在使用use-package的时候就不用加ensure了
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; ---------------------------------------------------------------------------
;;使用ripgrep来进行搜索（是在“~/.emacs”目录下搜索的）
;;ripgrep使用Rust语言编写的
;;consult-ripgrep
;;直接执行“M-x consult-ripgrep”会报错，说找不到“rg”。
;;解决办法：
;;1. 在网站“https://github.com/BurntSushi/ripgrep/releases”中下载对应macOS的压缩包（-apple-darwin.tar.gz）；
;;2. 下载完后解压，然后在压缩包中会找到一个可执行文件rg；
;;3. 将压缩包中的可执行文件rg复制到目录“/opt/homebrew/Cellar/emacs-mac/emacs-28.1-mac-9.0/libexec/emacs/28.1/aarch64-apple-darwin21.4.0”中。
;;⚠️
;;1. 执行rg的时候macOS会提示这个可执行文件未收到信任，这个时候直接全部信任就行了，应该不会出现什么问题。
;;2. rg文件的复制目录可能不完全一样，只要总体上的位置对就行
;;3. consult-ripgrep不支持中文查找
;;4. 有Grep和Find功能的consult命令（consult-ripgrep、consult-find、consult-locate等），可以利用在minibuffer中使用“#”筛选检索或查找的结果
;; ---------------------------------------------------------------------------


;; ---------------------------------------------------------------------------
;;visual-fill-column-mode
;;https://codeberg.org/joostkremers/visual-fill-column
(use-package visual-fill-column
  :ensure t
  :init

  ;;此处加“#”的目的是为了标记后边是函数，也可以不加
  (add-hook 'text-mode-hook 'toggle-truncate-lines-off)
  ;;在所有从text-mode衍生出来的mode中使用visual-fill-column-mode
  (add-hook 'text-mode-hook 'visual-fill-column-mode)

  :config
  (global-visual-fill-column-mode)
  (setq visual-fill-column-enable-sensible-window-split t)
  (setq-default visual-fill-column-center-text t)
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)
  ;;(setq-default fill-column 180)
  ;;visual-fill-column-extra-text-width可以调节文本在中间时，文本两边距屏幕边缘的距离
  (setq-default visual-fill-column-extra-text-width '(10 . 10))
  )

;; ---------------------------------------------------------------------------

;;代码格式.editorconfig
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;;unicode-fonts
(use-package unicode-fonts
  :config
  (unicode-fonts-setup))

;;evil模式
(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  ;;下面的代码可以将 insert state map 中的快捷键清空，使其可以回退（Fallback）到 Emacs State 中，
  ;;这样我们之前的 Emacs State 里面定义的 C-w 等快捷键就不会被 evil insert minor mode state 所覆盖。
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map [escape] 'evil-normal-state))

;; markdown
(use-package markdown-mode
  :ensure t)

(use-package restart-emacs)

(provide 'init-packages)
