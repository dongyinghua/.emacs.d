;;; init-doom-theme.el --- Initialize doom-theme configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; The configurations of doom-theme.
;;

;;; Code:

(require 'init-custom)

;;🔗https://github.com/doomemacs/themes
(use-package doom-themes
  :ensure t
  ;;:defer t
  ;;:hook (after-init . (lambda () (load-theme 'doom-one-light t)))
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; 比较好看的doom-theme：
  ;; 暗色系：doom-one、doom-xcode、doom-horizon、doom-molokai、doom-gruvbox、doom-monokai-pro
  ;; doom-henna
  ;; doom-opera感觉像是莫兰迪色系
  ;; 亮色系：doom-tomorrow-day、doom-opera-light、doom-one-light
  (setq-default my-theme 'doom-gruvbox)
  (load-theme my-theme t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq-default doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)

  ;; Emacs背景虚化
  ;; emacs-china：https://emacs-china.org/t/emacs-mac-port/15056/3
  ;; (set-face-background 'default "mac:windowBackgroundColor")
  ;; 如果下面关于背景虚化的配置代码没有放在use-package里，就会出现modeline中的图标之间出现间隙
  ;; 原因（只是猜测）可能是因为使用use-package来配置doom-themes，如果单独设置背景虚化，可能会出问题
  ;; (dolist (f (face-list)) (set-face-stipple f "alpha:60%"))
  ;;                                       (setq face-remapping-alist
  ;;                                         (append face-remapping-alist '((default my/default-blurred))))
  ;;                                       (defface my/default-blurred
  ;;                                         '((t :inherit 'default :stipple "alpha:60%"))
  ;;                                         "Like 'default but blurred."
  ;;                                         :group 'my)
  )

(provide 'init-doom-theme)

;;; init-doom-theme.el ends here.
