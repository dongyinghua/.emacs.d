;;; init-yafolding.el --- Initialize yafolding configurations.
;; -*- lexical-binding: t -*-

;;; Commentary:
;;
;; 代码折叠
;;

;;; Code:
(use-package yafolding
  :commands (yafolding-mode)
  :ensure t
  :hook (prog-mode . yafolding-mode)
  :config
  (setq yafolding-show-fringe-markers nil))

(provide 'init-yafolding)
;;; init-yafolding.el ends here
