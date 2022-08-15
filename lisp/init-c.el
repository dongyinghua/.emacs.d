;;; init-c.el --- Initialize c configurations.  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; C/C++ configuration.
;;

;;; Code:

;; C/C++ Mode
(use-package c++-mode
  :ensure nil
  :defer t
  :hook
  ((c-mode . lsp-deferred)
    (c++-mode . lsp-deferred))
  :bind (:map c-mode-base-map
          ("C-c c" . compile))
  :init (setq-default c-basic-offset 4)
  :config
  (use-package modern-cpp-font-lock
    :diminish
    :init (modern-c++-font-lock-global-mode t)))

(provide 'init-c)

;;; init-c.el ends here.
