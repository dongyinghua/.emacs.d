;;; init-java.el --- Initialize the java mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Java
(require 'init-funcs)
;; (use-package java-mode
;;   :ensure nil
;;   :defer t
;;   :hook(java-mode . (company-mode))
;;   ;; (java-mode . (if company-mode
;;   ;; 		   ((setq company-mode nil)
;;   ;; 		    (print("company is disabled")))
;;   ;; 		 (global-lsp-bridge-mode))
;;   ;; 	     )
;;   )

;;(add-hook 'java-mode-hook 'switch)
(provide 'init-java)
;;; init-java.el ends here.

