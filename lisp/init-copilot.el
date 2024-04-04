;;; init-copilot.el --- Initialize GitHub Copilot configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; GitHub Copilot configurations.
;; 参考：https://cloud.tencent.com/developer/article/2347983
;;

;;; Code:

(use-package copilot
  :load-path (lambda () (expand-file-name "copilot" dragonli-emacs-tools-file-path))
  :ensure nil
  :hook (prog-mode . copilot-mode)
  :config
  (setq copilot-indent-offset-warning-disable t) ;; 不打开这个会有警告，影响体验
  ;; complete by copilot first, then auto-complete
  (defun my-tab ()
    (interactive)
    (or (copilot-accept-completion)
	(ac-expand nil)))

  (with-eval-after-load 'auto-complete
    ;; disable inline preview
    (setq ac-disable-inline t)
    ;; show menu if have only one candidate
    (setq ac-candidate-menu-min 0))
  
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
  )

(provide 'init-copilot)
;;; init-copilot.el ends here
