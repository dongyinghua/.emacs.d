;;; package --- init-keybindings.el -*- lexical-binding: t -*-

;;; Commentary:
;;
;; seagle0128的配置中没有单独设置一个init-keybindings.el
;; TODO: 以后有机会参照seagle0128的配置把对快捷键的配置重新整理一下
;;

;;; Code:

(require 'init-funcs)

;;修改快捷键映射
(setq mac-option-modifier 'meta
      mac-command-modifier 'super)

;; ---------------------------------------------------------------------------
;;快捷键绑定
(global-set-key (kbd "s-a") 'mark-whole-buffer) ;;对应mac上面的Command-a 全选
(global-set-key (kbd "s-c") 'kill-ring-save) ;;对应mac上面的Command-c 复制
(global-set-key (kbd "s-s") 'save-buffer) ;; 对应mac上面的Command-s 保存
(global-set-key (kbd "s-v") 'yank) ;;对应mac上面的Command-v 粘贴
(global-set-key (kbd "s-z") 'undo) ;;对应mac上面的Command-z 撤销
(global-set-key (kbd "s-x") 'kill-region) ;;对应mac上面的Command-x 剪切
(global-set-key (kbd "s-q") 'save-buffers-kill-terminal) ;;对应mac上面的Command-q 退出应用

;;delete-frame
(global-set-key (kbd "s-w") 'delete-frame)

;;下面的这些函数可以让你找到不同函数，变量以及快捷键所定义的文件位置。
;;因为非常常用所以我们建议将其设置为与查找文档类似的快捷键（如下所示）
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)

;;这一行代码，将函数 open-init-file 绑定到 <f1> 键上
(global-set-key (kbd "<f1>") 'open-init-file)
(global-set-key (kbd "<f2>") 'open-init-org)

;; (global-unset-key (kbd "C-o"))
(define-key global-map (kbd "C-c C-e b") 'eval-buffer)
(define-key global-map (kbd "C-o") nil)
(define-key global-map (kbd "C-x C-；") 'comment-line)
(define-key global-map (kbd "M-》") 'end-of-buffer)
(define-key global-map (kbd "M-《") 'beginning-of-buffer)

(global-set-key (kbd "C-c f") 'dragonli-insert-in-line-formula-symbel-for-latex-formula)
(global-set-key (kbd "C-c c") 'dragonli-insert-cite-for-latex-formula)

;; (define-key global-map (kbd "s-s") 'dragonli-save-file)

(provide 'init-keybindings)
;;; init-keybindings.el ends here
