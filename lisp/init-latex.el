;;; init-latex.el --- Initialize latex configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Latex configurations.
;;

;;; Code:

(unless (package-installed-p 'auctex)
  (package-install 'auctex))

(setq-default TeX-engine 'xetex)

(provide 'init-latex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-latex.el ends here
