;;; init-avy.el --- Initialize avy configurations. -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:
;;
;; https://github.com/abo-abo/avy
;;

;;; Code:

(use-package avy
  :ensure t
  :defer t
  :bind (("C-c '" . avy-goto-char-timer)
          ("M-g l" . avy-goto-line)
          ("M-g w" . avy-goto-word-1)
          ("M-g o" . avy-org-goto-heading-timer)
	  ("M-g c" . avy-goto-char))
  :config
  (setq avy-timeout-seconds 2)
  (setf (alist-get ?e avy-dispatch-alist) 'avy-action-embark)
  (setq avy-all-windows nil
    avy-all-windows-alt t
    avy-background t
    avy-style 'pre))

(provide 'init-avy)
;;; init-avy.el ends here
