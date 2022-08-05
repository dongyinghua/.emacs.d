;;; init-evil.el --- Initialize evil configuration. -*- lexical-binding: t
;;; Commentary
;;; code:

;; evil模式
(use-package evil
  :ensure t
  :defer t
  :hook (after-init . evil-mode)
  :config
  ;; 下面的代码可以将 insert state map 中的快捷键清空，使其可以回退（Fallback）到 Emacs State 中，
  ;; 这样我们之前的 Emacs State 里面定义的 C-w 等快捷键就不会被 evil insert minor mode state 所覆盖。
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map [escape] 'evil-normal-state))

(provide 'init-evil)

;;; init-ecil.el ends here.
