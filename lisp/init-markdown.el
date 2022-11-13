;;; init-markdown.el --- Initialize Markdown configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; markdown
(use-package markdown-mode
  :ensure t
  :defer t
  :config
  (with-eval-after-load 'org
    (add-to-list 'org-export-backends 'md)))

(provide 'init-markdown)
;;; init-markdown.el ends here.
