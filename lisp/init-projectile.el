;; init-projectile.el --- Initialize projectile configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Projectile configurations.
;;

;;; Code:

(use-package projectile
  :ensure t
  :bind (:map projectile-mode-map
          ("s-p" . projectile-command-map)
          ("C-c p" . projectile-command-map))
  :hook (after-init . projectile-mode)
  :init
  (setq
    projectile-mode-line-prefix ""
    projectile-sort-order 'recentf
    projectile-use-git-grep t)
  :config
  ;; Use the faster searcher to handle project files: ripgrep `rg'.
  (when (and (not (executable-find "fd"))
          (executable-find "rg"))
    (setq projectile-generic-command
      (let ((rg-cmd ""))
        (dolist (dir projectile-globally-ignored-directories)
          (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
        (concat "rg -0 --files --color=never --hidden" rg-cmd)))))

;; (use-package counsel-projectile
;;   :ensure t
;;   :after projectile
;;   :init (counsel-projectile-mode))

(provide 'init-projectile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-projectile.el ends here
