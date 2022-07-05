;;; init-package.el --- Initialize package configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Emacs Package management configurations.
;;

;;; Code:

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

;; 这个配置一定要配置在 use-package 的初始化之前，否则无法正常安装
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
;;visual-fill-column-mode
;;https://codeberg.org/joostkremers/visual-fill-column
(use-package visual-fill-column
  :init
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
  :config
  (editorconfig-mode 1))

;;unicode-fonts
(use-package unicode-fonts
  :config
  (unicode-fonts-setup))

;;evil模式
(use-package evil
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

(use-package which-key
  :hook (after-init . which-key-mode)
  :config (setq which-key-idle-delay 0))

(provide 'init-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-packages.el ends here
