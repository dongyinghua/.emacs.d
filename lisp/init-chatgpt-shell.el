;;; init-org.el --- Initialize chatgpt-shell configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Chatgpt-shell configurations.
;;

;;; Code:

;; (use-package chatgpt-shell
;;   :ensure t
;;   :custom
;;   ((chatgpt-shell-api-url-base "https://api.openai.com")
;;    (chatgpt-shell-openai-key
;;     (lambda ()
;;       ;; Here the openai-key should be the proxy service key.
;;       (auth-source-pass-get 'secret "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")))))
(use-package chatgpt-shell
  :ensure t
  :custom
  ((chatgpt-shell-openai-key
    (lambda ()
      (auth-source-pass-get 'secret "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")))))

(provide 'init-chatgpt-shell)
;;; init-chatgpt-shell.el ends here
