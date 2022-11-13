;;; init-package.el --- Initialize package configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Emacs Package management configurations.
;;

;;; Code:

(require 'init-funcs)

;; 默认 dragonli-package-archives 是 ustc
;; (setq-default dragonli-package-archives 'melpa)
(set-package-archives dragonli-package-archives nil nil t)
;; 或者用如下代码修改镜像
;;(set-package-archives 'melpa nil nil t)

;;防止反复调用 package-refresh-contents 会影响加载速度
(when (not package-archive-contents)
  (package-refresh-contents))

;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; 这个配置一定要配置在 use-package 的初始化之前，否则无法正常安装
;; 目的是不使用emacs内置的org
(assq-delete-all 'org package--builtins)
(assq-delete-all 'org package--builtin-versions)

;; Setup `use-package'
;; (package-installed-p 'use-package) 如果没有安装use-package，就返回nil
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; 以后在使用use-package的时候就不用加ensure了
;;(require 'use-package-ensure)
;;(setq use-package-always-ensure t)


;; 关于hydra的配置应当写在靠前一点的位置比较保险。
(use-package hydra
  :ensure t)

(use-package pretty-hydra
  :ensure t
  :init
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                 &key face height v-adjust)
    "Add an icon in the hydra title."
    (let ((face (or face `(:foreground ,(face-background 'highlight))))
           (height (or height 1.0))
           (v-adjust (or v-adjust 0.0)))
      (concat
        (when (and (icon-displayable-p) icon-type icon-name)
          (let ((f (intern (format "all-the-icons-%s" icon-type))))
            (when (fboundp f)
              (concat
                (apply f (list icon-name :face face :height height :v-adjust v-adjust))
                " "))))
        (propertize title 'face face)))))



;; 查看 Emacs 启动时加载插件的时间
(use-package benchmark-init
  :ensure t)

;; ---------------------------------------------------------------------------
;;visual-fill-column-mode
;;https://codeberg.org/joostkremers/visual-fill-column
(use-package visual-fill-column
  :ensure t
  :defer t
  :hook (after-init . (lambda () (global-visual-fill-column-mode)))
  :init
  (add-hook 'text-mode-hook 'toggle-truncate-lines-off)
  ;; 在所有从text-mode衍生出来的mode中使用visual-fill-column-mode
  (add-hook 'text-mode-hook 'visual-fill-column-mode)
  :config
  (setq visual-fill-column-enable-sensible-window-split t)
  ;;(setq-default visual-fill-column-center-text t)
  (advice-add 'text-scale-adjust :after 'visual-fill-column-adjust)
  (setq-default fill-column 100)
  ;; visual-fill-column-extra-text-width可以调节文本在中间时，文本两边距屏幕边缘的距离
  ;;(setq-default visual-fill-column-extra-text-width '(5 . 10))
  )

;; ---------------------------------------------------------------------------

;;代码格式.editorconfig
(use-package editorconfig
  :ensure t
  :defer t
  :hook (after-init . editorconfig-mode)
  :bind ("C-c e" . editorconfig-format-buffer))

;; unicode-fonts
(use-package unicode-fonts
  :ensure t
  :defer t
  :hook (after-init . (lambda () (unicode-fonts-setup))))

(use-package restart-emacs
  :ensure t
  :defer t)

;;(use-package which-key
;;  :hook (after-init . which-key-mode)
;;  :config (setq which-key-idle-delay 0))

;; https://github.com/DarwinAwardWinner/amx
;; (use-package amx
;;   :ensure t
;;   :hook (after-init . amx-mode))

;; https://github.com/abo-abo/ace-window
(use-package ace-window
  :ensure t
  :defer t
  :bind (("C-x o" . 'ace-window)))

(use-package mwim
  :ensure t
  :defer t
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

(use-package undo-tree
  :ensure t
  :defer t
  :hook (after-init . (lambda () (global-undo-tree-mode)))
  :config
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-auto-save-history t))
;;(evil-set-undo-system 'undo-tree))

;; https://github.com/Malabarba/smart-mode-line
;;(use-package smart-mode-line
;;  :config (sml/setup))

;; https://github.com/abo-abo/avy
(use-package avy
  :ensure t
  :defer t
  :bind (("C-c '" . avy-goto-char-timer)
          ("M-g l" . avy-goto-line)
          ("M-g w" . avy-goto-word-1)
          ("M-g o" . avy-org-goto-heading-timer))
  :config
  (setq avy-timeout-seconds 2)
  (setf (alist-get ?e avy-dispatch-alist) 'avy-action-embark)
  (setq avy-all-windows nil
    avy-all-windows-alt t
    avy-background t
    avy-style 'pre))

;;(use-package tiny)

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

;; 删除很多空格使使用的
(use-package hungry-delete
  :ensure t
  :defer t
  :init
  (global-hungry-delete-mode)
  (setq hungry-delete-join-reluctantly t))


(provide 'init-packages)
;;; init-packages.el ends here
