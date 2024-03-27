;;; init-outline.el --- Initialize outline configurations. -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:
;;
;; Emacs内置的outline-mode
;;

;;; Code:

(defun outline-toggle-all ()
  "Show or hide the all of the text on its current state."
  (interactive)
  (save-excursion
    (outline-back-to-heading)
    (if (not (outline-invisible-p (line-end-position)))
        (outline-hide-body)
      (outline-show-all)
      )))

(use-package outline
  :ensure nil
  :hook
  (LaTeX-mode . outline-minor-mode)
  :init
  ;; (setq outline-minor-mode-cycle-filter "eolp")
  :config
  ;; (define-key outline-minor-mode-map (["C-o"]) outline-minor-mode-map)
  (setq outline-minor-mode-use-buttons t)
  (setq outline-minor-mode-cycle t)
  (define-key outline-minor-mode-map (kbd "C-o TAB") 'outline-toggle-children)
  (define-key outline-minor-mode-map (kbd "C-o C-n") 'outline-next-visible-heading)
  (define-key outline-minor-mode-map (kbd "C-o C-p") 'outline-previous-visible-heading)
  (define-key outline-minor-mode-map (kbd "C-o C-a") 'outline-toggle-all)
  
  (define-key outline-minor-mode-map (kbd "C-o C-,") 'outline-promote)
  (define-key outline-minor-mode-map (kbd "C-o C-，") 'outline-promote)
  (define-key outline-minor-mode-map (kbd "C-o C-.") 'outline-demote)
  (define-key outline-minor-mode-map (kbd "C-o C-。") 'outline-demote)
  )

(provide 'init-outline)
;;; init-outline.el ends here.

