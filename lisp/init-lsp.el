;;; init-lsp.el --- Initialize lsp configuration. -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary
;;
;; Refer to https://github.com/zilongshanren/emacs.d/blob/develop/lisp/init-lsp.el
;;

;;; Code:

(require 'init-custom)

(add-to-list 'load-path (expand-file-name "lsp-bridge" dragonli-emacs-tools-file-path))

(require 'yasnippet)
(yas-global-mode 1)

(require 'lsp-bridge)
(setq lsp-bridge-enable-log nil)

(when (string= my/completion-styles "lsp-bridge")
  (progn
    (global-lsp-bridge-mode)
    (setq-local evil-goto-definition-functions '(lsp-bridge-jump))
    (setq acm-candidate-match-function 'orderless-flex)))

;; Debug
(use-package dap-mode
  :defines dap-python-executable
  :functions dap-hydra/nil
  :diminish
  :bind (:map lsp-mode-map
          ("<f5>" . dap-debug)
          ("M-<f5>" . dap-hydra))
  :hook ((after-init     . dap-auto-configure-mode)
          (dap-stopped    . (lambda (_) (dap-hydra)))
          (dap-terminated . (lambda (_) (dap-hydra/nil)))

          (python-mode            . (lambda () (require 'dap-python)))
          ;;(ruby-mode              . (lambda () (require 'dap-ruby)))
          ;;(go-mode                . (lambda () (require 'dap-go)))
          (java-mode              . (lambda () (require 'dap-java)))
          ;;((c-mode c++-mode)      . (lambda () (require 'dap-lldb)))
          ((objc-mode swift-mode) . (lambda () (require 'dap-lldb)))
          ;;(php-mode               . (lambda () (require 'dap-php)))
          ;;(elixir-mode            . (lambda () (require 'dap-elixir)))
          ;;((js-mode js2-mode)     . (lambda () (require 'dap-chrome)))
          (powershell-mode        . (lambda () (require 'dap-pwsh))))
  :init
  (setq dap-auto-configure-features '(sessions locals breakpoints expressions controls))
  (when (executable-find "python3")
    (setq dap-python-executable "python3")))

(use-package dumb-jump
  :ensure t
  :hook (xref-backend-functions . dumb-jump-xref-activate))

;; make evil jump & jump back as expected
;; (defun evil-set-jump-args (&rest ns) (evil-set-jump))
;; (advice-add 'lsp-bridge-jump :before #'evil-set-jump-args)
(evil-add-command-properties #'lsp-bridge-jump :jump t)

;; 融合 `lsp-bridge' `find-function' 以及 `dumb-jump' 的智能跳转
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

(with-eval-after-load 'xref
  (setq xref-search-program 'ripgrep)     ;project-find-regexp
  (when (functionp 'xref-show-definitions-completing-read)
    (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
    (setq xref-show-xrefs-function #'xref-show-definitions-completing-read)))



(provide 'init-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
