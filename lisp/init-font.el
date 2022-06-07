;; -*- lexical-binding: t -*-
;; 问题：在当前shell下运行emacs 出现 fontset 'tty' does not exist.
;; 解决办法：https://www.programminghunter.com/article/25811307569/
(defun s-font()
  (interactive)
  ;;Setting English Font
  (set-face-attribute
    'default nil :font "Source Code Pro 20" :weight 'normal)
  ;;Chinese Font
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
      charset
      (font-spec :family "冬青黑体简体中文 W6")))
  ;; tune rescale so that Chinese character width = 2 * English character width
  (setq face-font-rescale-alist '(("monospace" . 1.0) ("WenQuanYi" . 1.23)))
  )

(add-to-list 'after-make-frame-functions
  (lambda (new-frame)
    (select-frame new-frame)
    (if window-system
      (s-font))))
(if window-system
  (s-font))

(provide 'init-font)
