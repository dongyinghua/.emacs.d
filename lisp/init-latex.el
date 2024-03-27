;;; init-latex.el --- Initialize latex configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Latex configurations.
;;

;;; Code:

(unless (package-installed-p 'auctex)
  (package-install 'auctex))

(setq-default TeX-engine 'xetex)

(use-package cdlatex
  ;;:load-path (lambda () (expand-file-name "cdlatex" dragonli-emacs-tools-file-path))
  ;;"~/.emacs.d/tools/cdlatex"
  :ensure t
  :defer t
  :init
  ;; (add-hook 'org-mode-hook #'turn-on-org-cdlatex)
  (add-hook 'org-mode-hook #'turn-on-cdlatex)
  (add-hook 'LaTeX-mode-hook #'turn-on-cdlatex)   ; with AUCTeX LaTeX mode
  (add-hook 'latex-mode-hook #'turn-on-cdlatex)   ; with Emacs latex mode
  (add-hook 'tex-mode-hook 'turn-on-cdlatex)   ; with Emacs latex mode
  :config
  (define-key cdlatex-mode-map (kbd "TAB") nil)
  (define-key cdlatex-mode-map (kbd "C-<tab>") 'cdlatex-tab)
  )

(provide 'init-latex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-latex.el ends here
