;;; init-origami.el --- Initialize origami configurations.
;; -*- lexical-binding: t -*-

;;; Commentary:
;;
;; 代码折叠
;;

;;; Code:
(use-package origami
  :ensure t
  :bind	("C-<tab>" . origami-toggle-node)
  :config
  (global-origami-mode)
  (define-key origami-mode-map (kbd "C-<return>") 'origami-toggle-node)
  )

(provide 'init-origami)
;;; init-origami.el ends here
