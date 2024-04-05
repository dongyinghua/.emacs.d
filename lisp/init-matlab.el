;;; init-matlab.el --- Initialize matlab configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; matlab configurations.
;;

;;; Code:
(use-package matlab-mode
  :ensure t
  :defer t
  :init
  (autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
  (add-to-list
   'auto-mode-alist
   '("\\.m$" . matlab-mode))
  (setq-default matlab-indent-function t)
  (setq-default matlab-shell-command "/Applications/MATLAB_R2023b.app/bin/matlab")
  (setq-default matlab-shell-command-switches (list "-nodesktop"))
  )

;; (autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
;; (add-to-list
;;  'auto-mode-alist
;;  '("\\.m$" . matlab-mode))
;; (setq matlab-indent-function t)
;; (setq matlab-shell-command "/Applications/MATLAB_R2023b.app/bin/matlab")
;; (setq matlab-shell-command-switches (list "-nodesktop"))

(provide 'init-matlab)
;;; init-matlab.el ends here.
