;;; init-font.el --- Initialize font configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; 问题：在当前shell下运行emacs 出现 fontset 'tty' does not exist.
;; 解决办法：https://www.programminghunter.com/article/25811307569/
;;

;;; Code:

(require 'init-funcs)

;; (add-to-list 'after-make-frame-functions
;;   (lambda (new-frame)
;;     (select-frame new-frame)
;;     (if window-system
;;       (s-font))))

(if window-system
  (s-font))

(provide 'init-font)
;;; init-font.el ends here
