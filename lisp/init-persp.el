;;; init-persp.el --- Initialize perspectives configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; perspectives configurations.
;;

;;; Code:
(use-package persp-mode
  :hook (after-init . persp-mode)
  :init (setq persp-keymap-prefix (kbd "C-x p")
              persp-nil-name "default"
              persp-set-last-persp-for-new-frames nil
              persp-kill-foreign-buffer-behaviour 'kill
              persp-auto-resume-time (if centaur-dashboard 0 1.0)))

(provide 'init-persp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-persp.el ends here
