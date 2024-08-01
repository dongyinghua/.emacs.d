;; init-fanyi.el --- Initialize fanyi configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Using fanyi in Emacs.
;;

;;; Code:
;; Default, comment out the providers you don't need.
(use-package fanyi
  :ensure t
  :custom
  (fanyi-providers '(;; 海词
                     fanyi-haici-provider
                     ;; 有道同义词词典
                     fanyi-youdao-thesaurus-provider
                     ;; Etymonline
                     ;; fanyi-etymon-provider
                     ;; Longman
                     ;; fanyi-longman-provider
		     ))
  :config
  (setq fanyi-sound-player-support-https t)
  )

(provide 'init-fanyi)
;;; init-fanyi.el ends here
