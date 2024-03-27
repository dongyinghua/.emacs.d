;;; init-pdf-tools.el --- Initialize pdf-tools configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; pdf-tools configurations.
;;

;;; Code:

(use-package pdf-tools
  :ensure t
  :defer t
  :config
  (pdf-loader-install)

  ;; 配合 AucTeX 使用的配置
  (setq TeX-PDF-mode t)
  (setq TeX-source-correlate-mode t) ;; 编译后开启正反向搜索
  (setq TeX-source-correlate-method 'synctex) ;; 正反向搜索的执行方式
  (setq TeX-source-correlate-start-server t) ;; 不再询问是否开启服务器以执行反向搜索

  
  (setq native-comp-deferred-compilation-deny-list '(".*pdf.*"))
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))) ;; 用pdf-tools 打开 pdf
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer) ;; 在完成编译后刷新 pdf 文件

  (define-key pdf-view-mode-map "d" 'pdf-view-next-page-command) ;; 向后翻页
  (define-key pdf-view-mode-map "a" 'pdf-view-previous-page-command) ;; 向前翻页
  (define-key pdf-view-mode-map "s" 'pdf-view-scroll-up-or-next-page) ;; 向下滑动
  (define-key pdf-view-mode-map "w" 'pdf-view-scroll-down-or-previous-page) ;; 向上滑动

  (add-hook 'pdf-view-mode-hook 'pdf-view-fit-width-to-window) ;; 自动放大到页宽

  )

(provide 'init-pdf-tools)
;;; init-pdf-tools.el ends here.
