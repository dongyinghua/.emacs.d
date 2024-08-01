;;; init-encoding.el --- Encoding configurations. -*- lexical-binding: t -*-
;; 设置Emacs的编码

;;; Commentary:
;;
;; 借鉴：https://github.com/hick/emacs-chinese?tab=readme-ov-file
;;

;;; Code:

;; 设置编码
;; (setq default-buffer-file-coding-system 'utf-8)
;; (set-default-coding-systems 'utf-8)

;; (set-default-coding-systems 'utf-8)

;; (set-language-environment "UTF-8")

;; (set-buffer-file-coding-system 'utf-8)

(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)
(setq system-time-locale "C")

(provide 'init-encoding)
;;; init-encoding.el ends here
