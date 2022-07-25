;;; init-git.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:

;; See also init-github.el.

;;; Code:

;; git
(use-package magit
  :ensure t
  :hook (after-init . magit-mode))

(provide 'init-git)
;;; init-git.el ends here
