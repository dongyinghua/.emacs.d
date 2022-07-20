;; init-projectile.el --- Initialize projectile configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Projectile configurations.
;;

;;; Code:

(use-package projectile
  :ensure t
  :config
  (projectile-mode t)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-mode-line "Projectile")
  (setq projectile-track-known-projects-automatically nil))

(use-package counsel-projectile
  :ensure t
  :after projectile
  :init (counsel-projectile-mode))

(provide 'init-projectile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-projectile.el ends here
