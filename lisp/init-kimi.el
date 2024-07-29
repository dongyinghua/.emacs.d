;;; init-kimi.el --- Initialize Kimi configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Kimi configurations.
;; 参考：https://emacs-china.org/t/kimi-chat/26826/21?page=2
;;

;;; Code:

(use-package gptel
  :ensure t
  :config
  (setq gptel-use-curl nil)          ;; 我觉得这一句是关键，供你参考
  (setq gptel-default-mode 'org-mode)
  
  (setq gptel-backend
        (gptel-make-openai "Kimi"               ;Any name you want
	  :host "api.moonshot.cn"
	  :endpoint "/v1/chat/completions"
	  :stream t
	  :key "sk-FmVCh1wbMhEzNNJqVv4ke0B4gtL7m5GgmN98jOwyJUjL56v8"
	  :models '("moonshot-v1-8k" "moonshot-v1-16k"))
	)
  )

(provide 'init-kimi)
;;; init-kimi.el ends here
