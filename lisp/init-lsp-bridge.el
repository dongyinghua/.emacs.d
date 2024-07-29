;;; init-lsp.el --- Initialize lsp configuration. -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(require 'init-custom)
(require 'init-funcs)

;; (add-to-list 'load-path (expand-file-name "lsp-bridge" dragonli-emacs-tools-file-path))
;;(require 'lsp-bridge)

(use-package lsp-bridge
  :load-path (lambda () (expand-file-name "lsp-bridge" dragonli-emacs-tools-file-path))
  :ensure nil
  :defer t
  :hook
  (emacs-lisp-mode . lsp-bridge-mode)
  (java-mode . lsp-bridge-mode)
  (Java//l-mode . lsp-bridge-mode)
  (matlab-mode . lsp-bridge-mode)
  (python-mode . lsp-bridge-mode)
  (cc-mode . lsp-bridge-mode)
  ;; :bind
  ;; ("C-c C-l d" . lsp-bridge-find-define)
  ;; ("C-c C-l j" . lsp-bridge-jump)
  ;; ("C-c C-l b" . lsp-bridge-jump-back)
  :init
  ;;(setq lsp-bridge-enable-log nil)
  ;; (require 'lsp-bridge)
  ;;(global-lsp-bridge-mode)
  :config
  ;; (setq lsp-bridge-python-command "/opt/miniconda3/bin/python3")
  ;; (lsp-bridge-install-tabnine)
  (setq acm-enable-tabnine t)
  (add-to-list 'lsp-bridge-single-lang-server-mode-list '(matlab-mode . "matlab-ls"))
  (add-to-list 'lsp-bridge-default-mode-hooks 'matlab-mode-hook)
  (add-to-list 'lsp-bridge-formatting-indent-alist '(matlab-mode . matlab-indent-level))
  )

(provide 'init-lsp-bridge)
;;; init-lsp-bridge.el ends here.
