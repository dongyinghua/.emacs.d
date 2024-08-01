;;; init-exec-path.el --- Set up exec-path to help Emacs find programs  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package exec-path-from-shell
  ;; :if (memq window-system '(mac ns))
  :ensure t
  :init
  (setq exec-path-from-shell-arguments nil)
  ;; (exec-path-from-shell-initialize)
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  )

;; (use-package exec-path-from-shell
;;   :if (memq window-system '(mac ns))
;;   :ensure t
;;   :hook (after-init . (lambda () (exec-path-from-shell-initialize)))
;;   :init
;;   (setq exec-path-from-shell-arguments nil))

(provide 'init-exec-path)
;;; init-exec-path.el ends here
