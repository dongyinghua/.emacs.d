;;; init-flycheck.el --- Initialize flycheck configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Flycheck configurations.
;;

;;; Code:

(use-package flycheck
  :ensure t
  :hook
  (prog-mode . flycheck-mode))

(provide 'init-flycheck)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-flycheck.el ends here
