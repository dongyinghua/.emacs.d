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
  :interpreter ("python3" . python-mode))

(provide 'init-python)
;;; init-python.el ends here
