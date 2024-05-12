;;; init-python.el --- Initialize python configurations.  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Python configurations.
;;

;;; Code:
;; ;; Python Mode
(use-package python
  :ensure nil
  :defer t
  :mode ("\\.py\\'" . python-mode)
  ;; :interpreter ("/opt/miniconda3/bin/python3" . python-mode)
  :config
  ;; (setq python-interpreter "/opt/miniconda3/bin/python3")
  ;; (setq python-shell-interpreter "/opt/miniconda3/bin/python3")
  )

(provide 'init-python)
;;; init-python.el ends here
