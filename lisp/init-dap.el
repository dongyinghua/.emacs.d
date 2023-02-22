;;; init-dap.el --- Initialize the dap mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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

            ;; ((python-mode python-ts-mode)            . (lambda () (require 'dap-python)))
            ;; ((ruby-mode ruby-ts-mode)                . (lambda () (require 'dap-ruby)))
            ;; ((go-mode go-ts-mode)                    . (lambda () (require 'dap-go)))
            ((java-mode java-ts-mode jdee-mode)      . (lambda () (require 'dap-java)))
            ((c-mode c-ts-mode c++-mode c++-ts-mode) . (lambda () (require 'dap-lldb)))
            ;; ((objc-mode swift-mode)                  . (lambda () (require 'dap-lldb)))
            ;; (php-mode                                . (lambda () (require 'dap-php)))
            ;; (elixir-mode                             . (lambda () (require 'dap-elixir)))
            ;; ((js-mode js2-mode js-ts-mode)           . (lambda () (require 'dap-chrome)))
            ;;(powershell-mode                         . (lambda () (require 'dap-pwsh)))
	    )
     :init (when (executable-find "python3")
             (setq dap-python-executable "python3")))



(provide 'init-dap)
;;; init-dap.el ends here.
