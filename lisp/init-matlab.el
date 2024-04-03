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
  (setq matlab-indent-function t)
  ;; % CLI matlab from the shell:
  ;; % /Applications/MATLAB_R2016a.app/bin/matlab -nodesktop
  ;; %
  ;; % elisp setup for matlab-mode:
  (setq matlab-shell-command "/Applications/MATLAB_R2023b.app/bin/matlab")
  (setq matlab-shell-command-switches (list "-nodesktop"))
  )

(provide 'init-matlab)
;;; init-matlab.el ends here.
