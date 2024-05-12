;;; init-origami.el --- Initialize origami configurations.
;; -*- lexical-binding: t -*-

;;; Commentary:
;;
;; 代码折叠
;;

;;; Code:
(use-package origami
  :ensure t
  :hook (prog-mode . origami-mode)
  :bind	("C-<tab>" . origami-toggle-node)
  :config
  (global-origami-mode)
  )

(provide 'init-origami)
;;; init-origami.el ends here
