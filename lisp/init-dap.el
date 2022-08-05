;;; init-dap.el --- Initialize the dap mode. -*- lexical-binding: t -*-
;;;Commentary
;;; Code:

(use-package dap-mode
  :ensure t
  :after hydra lsp-mode
  :init
  (dap-mode 1)
  (dap-tooltip-mode 1)
  (dap-auto-configure-mode 1)
  (dap-ui-controls-mode 1)
  (require 'dap-lldb)
  :config
  (use-package dap-ui
    :ensure nil
    :config
    (dap-ui-mode 1))

  (setq dap-auto-configure-feature
    '(sessions locals breakpoints expressions repl controls tooltip)))

;;(use-package dap-lldb)

(provide 'init-dap)
;;; init-dap.el ends here.
