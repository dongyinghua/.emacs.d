;;; init-latex.el --- Initialize latex configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Latex configurations.
;;

;;; Code:

(setq-default TeX-engine 'xetex)
;; (setq-default org-latex-compiler "xelatex")
;; (setq-default org-latex-pdf-process '("xelatex %f"))
;;(add-to-list 'org-latex-default-packages-alist '("" "ctex" t ("xelatex")))

(provide 'init-latex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-latex.el ends here
