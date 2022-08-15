;;; init-dap.el --- Initialize the dap mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Debug
(use-package dap-mode
  ;;:defines dap-python-executable
  :ensure t
  :defer t
  :bind (:map lsp-mode-map
          ("<f5>" . dap-debug)
          ("M-<f5>" . dap-hydra))
  :hook ((after-init     . dap-auto-configure-mode)
          (dap-stopped    . (lambda (_) (dap-hydra)))

          (python-mode            . (lambda () (require 'dap-python)))
          ;;(ruby-mode              . (lambda () (require 'dap-ruby)))
          ;;(go-mode                . (lambda () (require 'dap-go)))
          ;;(java-mode              . (lambda () (require 'dap-java)))
          ((c-mode c++-mode)      . (lambda () (require 'dap-lldb)))
          ;;((objc-mode swift-mode) . (lambda () (require 'dap-lldb)))
          ;;(php-mode               . (lambda () (require 'dap-php)))
          ;;(elixir-mode            . (lambda () (require 'dap-elixir)))
          ;;((js-mode js2-mode)     . (lambda () (require 'dap-chrome)))
          ;;(powershell-mode        . (lambda () (require 'dap-pwsh)))
          )
  :init
  (setq dap-auto-configure-features '(sessions locals breakpoints expressions controls))
  (when (executable-find "python3")
    (setq-default dap-python-executable "python3"))
  )


(provide 'init-dap)
;;; init-dap.el ends here.
