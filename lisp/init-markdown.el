;;; init-markdown.el --- Initialize Markdown configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; markdown
;; (require 'package)
;; (add-to-list 'package-archives
             ;; '("melpa-stable" . "https://stable.melpa.org/packages/"))
;; (package-initialize)

(use-package markdown-mode
  :ensure t
  :defer t
  :mode ("README\\.md\\'" . gfm-mode)
  :config
  (with-eval-after-load 'org
    (add-to-list 'org-export-backends 'md)))

(provide 'init-markdown)
;;; init-markdown.el ends here.
