;;; init.tools.el --- Initialize special tools configurations.
;; -*- lexical-binding: t -*-

;;; Commentary:
;;
;; init-tools.el 放置一些需要从网上下载package并手动放到.emacs.d中的插件
;;

;;; Code:

(require 'init-custom)

;; (add-to-list 'load-path (expand-file-name "awesome-tab" dragonli-emacs-tools-file-path))
;; (add-to-list 'load-path (expand-file-name "org-bars" dragonli-emacs-tools-file-path))

(use-package awesome-tab
  :load-path (lambda () (expand-file-name "awesome-tab" dragonli-emacs-tools-file-path))
  ;;"~/.emacs.d/tools/awesome-tab"
  :ensure nil
  :defer t
  :hook (after-init . awesome-tab-mode)
  :config
  ;; (require 'awesome-tab)
  ;; (awesome-tab-mode t)
  ;; 调整宽度
  (setq awesome-tab-height 100)
  
  ;;   (defun awesome-tab-buffer-groups ()
  ;;     "`awesome-tab-buffer-groups' control buffers' group rules.
  ;; Group awesome-tab with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
  ;; All buffer name start with * will group to \"Emacs\".
  ;; Other buffer group by `awesome-tab-get-group-name' with project name."
  ;;     (list
  ;;       (cond
  ;;         ((or (string-equal "*" (substring (buffer-name) 0 1))
  ;;            (memq major-mode '(magit-process-mode
  ;;                                magit-status-mode
  ;;                                magit-diff-mode
  ;;                                magit-log-mode
  ;;                                magit-file-mode
  ;;                                magit-blob-mode
  ;;                                magit-blame-mode)))
  ;;           "Emacs")
  ;;         ((derived-mode-p 'eshell-mode)
  ;;           "EShell")
  ;;         ((derived-mode-p 'dired-mode)
  ;;           "Dired")
  ;;         ((memq major-mode '(org-mode org-agenda-mode diary-mode))
  ;;           "OrgMode")
  ;;         ((derived-mode-p 'eaf-mode)
  ;;           "EAF")
  ;;         (t
  ;;           (awesome-tab-get-group-name (current-buffer))))))
  )

(use-package org-bars
  :load-path (lambda () (expand-file-name "org-bars" dragonli-emacs-tools-file-path))
  ;;"~/.emacs.d/tools/org-bars"
  :ensure nil
  :defer t
  :hook (org-mode . org-bars-mode)
  )

(provide 'init-tools)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-tools.el ends here
