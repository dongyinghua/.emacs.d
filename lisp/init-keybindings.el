;; -*- lexical-binding: t -*-

;;修改快捷键映射
(setq mac-option-modifier 'meta
  mac-command-modifier 'super)

;;===========================================================================
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

;;;;“C-x C-h”可以看到所有以“C-x”开头的快捷键
;;“C-c”是Emacs给用户保留的快捷键前缀，“C-x”是给系统的
;;尽量全部用“C-c”作为自定义快捷键的前缀，这样既可以用embark来直接在minibuffer中执行命令
;;“C-c C-h”可以查看以“C-c”为前缀的快捷键，然后在embark加持下，就可以选择快捷键来直接执行命令
;;同理“C-x C-h”也可以用这种方式
(global-set-key (kbd "C-c e") 'editorconfig-format-buffer)

;;下面的这些函数可以让你找到不同函数，变量以及快捷键所定义的文件位置。
;;因为非常常用所以我们建议将其设置为与查找文档类似的快捷键（如下所示）
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)

(global-set-key (kbd "C-c f") 'org-roam-node-find)

;;快速打开配置文件
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
;;这一行代码，将函数 open-init-file 绑定到 <f1> 键上
(global-set-key (kbd "<f1>") 'open-init-file)

(defun open-init-org()
  (interactive)
  (find-file "~/.emacs.d/lisp/init-org.el"))
(global-set-key (kbd "<f2>") 'open-init-org)

(global-set-key (kbd "C-c C-e b") 'eval-buffer)

(provide 'init-keybindings)
