;;; init-avy.el --- Initialize avy configurations. -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:
;;
;; https://github.com/abo-abo/avy
;;

;;; Code:

;; minibuffer模糊查找
(use-package orderless
  :ensure t
  :defer t
  :init
  (setq completion-styles '(orderless))
  )

(provide 'init-orderless)
;;; init-orderless.el ends here
