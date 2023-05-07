;;; init-lsp.el --- Initialize lsp configuration. -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(require 'init-custom)
(require 'init-funcs)

(add-to-list 'load-path (expand-file-name "lsp-bridge" dragonli-emacs-tools-file-path))
;;(require 'lsp-bridge)


(use-package lsp-bridge
  :ensure nil
  :defer t
  :hook
  (emacs-lisp-mode . lsp-bridge-mode)
  (java-mode . lsp-bridge-mode)
  :bind
  ("C-c C-l d" . lsp-bridge-find-define)
  ("C-c C-l j" . lsp-bridge-jump)
  ("C-c C-l b" . lsp-bridge-jump-back)
  :init
  ;;(setq lsp-bridge-enable-log nil)
  (require 'lsp-bridge)
  ;;(global-lsp-bridge-mode)
  :config
  ;;(lsp-bridge-install-tabnine)
  ;;(setq acm-enable-tabnine t)
  
  ;; 融合 `Lsp-bridge' `find-function' 以及 `dumb-jump' 的智能跳转
  (defun lsp-bridge-jump ()
    (interactive)
    (cond
     ((eq major-mode 'emacs-lisp-mode)
      (evil-goto-definition))
     ((eq major-mode 'org-mode)
      (org-agenda-open-link))
     (lsp-bridge-mode
      (lsp-bridge-find-def))
     (t
      (require 'dumb-jump)
      (dumb-jump-go))))
  
  (defun lsp-bridge-jump-back ()
    (interactive)
    (cond
     (lsp-bridge-mode
      (lsp-bridge-return-from-def))
     (t
      (require 'dumb-jump)
      (dumb-jump-back))))
  )


(provide 'init-lsp-bridge)
;;; init-lsp-bridge.el ends here.
