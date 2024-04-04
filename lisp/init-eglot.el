;;; init-eglot.el --- eglot-mode configuration

;;; Commentary:
;;
;; eglot-mode configuration

;;; Code:
(use-package eglot
  :ensure t
  :defer t
  :hook (matlab-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(matlab-mode . ("~/.emacs.d/lisp/matlab_ls.json" "-stdio")))
  )
(provide 'init-eglot)
;;; init-eglot.el ends here
