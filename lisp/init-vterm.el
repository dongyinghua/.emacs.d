;; init-eshell.el --- Initialize eshell configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Eshell configurations.
;;
;;; Code:
(use-package vterm
  :when (memq window-system '(mac ns x pgtk))
  :ensure t
  :defer t
  :bind (:map vterm-mode-map
              ("C-y" . vterm-yank)
              ("M-y" . vterm-yank-pop)
              ("C-k" . vterm-send-C-k-and-kill))
  :init
  (setq vterm-shell "zsh")
  :config
  (setq vterm-always-compile-module t)
  (defun vterm-send-C-k-and-kill ()
    "Send `C-k' to libvterm, and put content in kill-ring."
    (interactive)
    (kill-ring-save (point) (vterm-end-of-line))
    (vterm-send-key "k" nil nil t)))

(use-package vterm-toggle
  :when (memq window-system '(mac ns x pgtk))
  :ensure t
  :defer t
  :bind (([f8] . vterm-toggle)
         ([f9] . vterm-compile)
         :map vterm-mode-map
         ([f8] . vterm-toggle)
         ([(control return)] . vterm-toggle-insert-cd))
  :config
  (setq vterm-toggle-cd-auto-create-buffer nil)
  (defvar vterm-compile-buffer nil)
  (defun vterm-compile ()
    "Compile the program including the current buffer in `vterm'."
    (interactive)
    (setq compile-command (compilation-read-command compile-command))
    (let ((vterm-toggle-use-dedicated-buffer t)
          (vterm-toggle--vterm-dedicated-buffer (if (vterm-toggle--get-window)
                                                    (vterm-toggle-hide)
                                                  vterm-compile-buffer)))
      (with-current-buffer (vterm-toggle-cd)
        (setq vterm-compile-buffer (current-buffer))
        (rename-buffer "*vterm compilation*")
        (compilation-shell-minor-mode 1)
        (vterm-send-M-w)
        (vterm-send-string compile-command t)
        (vterm-send-return)))))

(provide 'init-vterm)
;;; init-vterm.el ends here.
