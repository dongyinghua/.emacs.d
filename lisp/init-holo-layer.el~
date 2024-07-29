;;; init-holo-layer.el --- holo-layer Configuration. -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:
;; https://github.com/manateelazycat/holo-layer/tree/master
;; 参考：https://emacs-china.org/t/emacs-neovide/24677/28
;;


;;; Code:
;; add sort-tab to your load-path
;; (add-to-list 'load-path (expand-file-name "sort-tab" dragonli-emacs-tools-file-path))
;; (require 'sort-tab)
;; (sort-tab-mode 1)

(use-package holo-layer
  :load-path (lambda () (expand-file-name "holo-layer" dragonli-emacs-tools-file-path))
  :ensure nil
  :defer t
  :hook (after-init . holo-layer-enable)
  :config
  ;; ⚠️ 如果用conda的话，需要把holo-layer-python-command 设成对应的python路径
  (setq holo-layer-python-command "/opt/miniconda3/bin/python3") ;
  ;; 开启果冻光标的动画效果
  (setq holo-layer-enable-cursor-animation t)
  ;; 开启彩虹缩进功能
  ;; (setq holo-layer-enable-indent-rainbow t)
  ;; 在屏幕右上角显示光标处信息，比如光标处单词的翻译
  ;; ⚠️如果开启了这个功能，就不能开启emacs的启动最大化，暂时还没有找到解决办法
  (setq holo-layer-enable-place-info t)
  ;; 果冻光标的颜色
  (setq holo-layer-cursor-color "#b8bb26")
  ;; 显示窗口边框
  ;; (setq holo-layer-enable-window-border t)
  ;; 激活窗口的边框颜色
  ;; (setq holo-layer-active-window-color t)
  ;; 光标处信息的字体大小
  (setq holo-layer-place-info-font-size 16)
  ;; 渲染现代标签栏，默认是 nil, 需要先安装 sort-tab
  ;; (setq holo-layer-sort-tab-ui t)
  )

(provide 'init-holo-layer)
;;; init-holo-layer.el ends here
