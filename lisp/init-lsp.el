;;; init-lsp.el --- Initialize lsp configuration. -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; Python
(use-package lsp-mode
  :ensure t
  :defer t
  :hook (after-init . lsp-mode)
  :config
  (use-package lsp-ui
    :ensure t
    :defer t
    :config
    (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
    (setq lsp-ui-doc-position 'top))
  )

(use-package lsp-treemacs
  :ensure t
  :defer t)

(use-package lsp-pyright
  :ensure t
  :defer t
  :preface
  ;; Use yapf to format
  (defun lsp-pyright-format-buffer ()
    (interactive)
    (when (and (executable-find "yapf") buffer-file-name)
      (call-process "yapf" nil nil nil "-i" buffer-file-name)))
  :hook (python-mode . (lambda ()
                         (lsp-deferred)
                         (require 'lsp-pyright)
                         (add-hook 'after-save-hook #'lsp-pyright-format-buffer t t)))
  :init
  (when (executable-find "python3")
    (setq lsp-pyright-python-executable-cmd "python3")))

(provide 'init-lsp)
;;; init-lsp.el ends here
