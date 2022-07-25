;;; init-dap.el --- Initialize the dap mode. -*- lexical-binding: t -*-
;;;Commentary
;;; Code:

(use-package dap-mode
  :ensure t
  :init
  (dap-mode 1)
  (dap-tooltip-mode 1)
  (dap-auto-configure-mode 1)
  (dap-ui-controls-mode 1)
  :config
  (setq dap-auto-configure-feature
    '(sessions locals breakpoints expressions repl controls tooltip))
  (setq dap-lldb-debug-program '(expand-file-name "")))

(provide 'init-dap)
;;; init-dap.el ends here.
