;;; init-basic.el --- Better default configurations. -*- lexical-binding: t -*-
;; 增强Emacs内置功能

;;; Commentary:
;;
;; Better defaults.
;;

;;; Code:

(server-start)

;; 优化启动速度
(add-hook 'after-init-hook
	  #'(lambda ()
	      ;; modeline上显示我的所有的按键和执行的命令
	      ;;(keycast-mode t)

	      ;; 取消emacs的自动备份
	      (setq make-backup-files nil)

	      ;; minibuffer模糊查找
	      (setq completion-styles '(orderless))

	      ;; 让鼠标滚动更好用
	      (setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
	      (setq mouse-wheel-progressive-speed nil)

	      ;; 也许你并不喜欢听到错误时的“哔哔”的警告提示音，使用下面的代码你可以关闭 Emacs 中的警告音
	      (setq ring-bell-function 'ignore)

	      ;; 简化Emacs需要用户确认命令是否执行时的“yes or no”
	      (fset 'yes-or-no-p 'y-or-n-p)

	      ;; 鼠标滚动
	      ;; Vertical Scroll
	      (setq scroll-step 1)
	      (setq scroll-margin 1)
	      (setq scroll-conservatively 101)
	      (setq scroll-up-aggressively 0.01)
	      (setq scroll-down-aggressively 0.01)
	      (setq auto-window-vscroll nil)
	      (setq fast-but-imprecise-scrolling nil)
	      (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
	      (setq mouse-wheel-progressive-speed nil)
	      ;; Horizontal Scroll
	      (setq hscroll-step 1)
	      (setq hscroll-margin 1)
	      ))


(define-advice show-paren-function (:around (fn) fix-show-paren-function)
  "Highlight enclosing parens.
Emacs 有一个自带的 package 来高亮括号，那就是 `show-paren-mode'，
但它只会在编辑器的光标处在括号上时才会生效，我们可以使用子龙山人的代码来使光标在括号内时高亮括号。"
  (cond ((looking-at-p "\\s(") (funcall fn))
	(t (save-excursion
             (ignore-errors (backward-up-list))
             (funcall fn)))))

;; 括号匹配
(use-package electric-pair
  :ensure nil
  :defer t
  :hook (after-init . electric-pair-mode)
  :config
  (setq-default electric-pair-pairs '(
                                      (?\{ . ?\})
                                      (?\“ . ?\”)
                                      )))

;; 历史文件
;; consult-buffer 中也有历史文件
(use-package recentf
  :ensure nil
  :defer t
  :hook (after-init . recentf-mode)
  :config
  ;;(recentf-mode t)
  (setq-default recentf-max-menu-item 50)
  (setq recentf-max-saved-items 50))

;; 使用下面的配置文件将删除功能配置成与其他图形界面的编辑器相同，
;; 即当你选中一段文字之后输入一个字符会替换掉你选中部分的文字。
(use-package delete-selection
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; 下面的代码可以使 Emacs 自动加载外部修改过的文件。
(global-auto-revert-mode)

;; savehist-mode记住之前使用Emacs的工作状态（例如使用M-x的一些命令）
;; 记录到history文件中
;; 因为设置了use-package的ensure的默认值是t，所以在使用use-package对Emacs内置package进行配置时要加上“：ensure: t”
(use-package savehist
  :ensure nil
  :defer t
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              history-length 1000
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 300)
  )

;; 记住光标位置
(use-package saveplace
  :ensure nil
  :defer t
  :hook (after-init . save-place-mode)) ; 这句的意思是在Emacs所有配置文件加载完成后（after-init）再加载save-place-mode

(use-package bookmark
  :ensure nil
  :config
  ;; Syncing Bookmarks with zsh
  ;; (defadvice bookmark-write-file
  ;;   (after local-directory-bookmarks-to-zsh-advice activate)
  ;;   (local-directory-bookmarks-to-zsh))

  ;; (defun local-directory-bookmarks-to-zsh ()
  ;;   (interactive)
  ;;   (when (and (require 'tramp nil t)
  ;;           (require 'bookmark nil t))
  ;;     (set-buffer (find-file-noselect "~/.zsh.bmk" t t))
  ;;     (delete-region (point-min) (point-max))
  ;;     (insert "# -*- mode:sh -*-\n")
  ;;     (let (collect-names)
  ;;       (mapc (lambda (item)
  ;;               (let ((name (replace-regexp-in-string "-" "_" (car item)))
  ;;                      (file (cdr (assoc 'filename
  ;;                                   (if (cddr item) item (cadr item))))))
  ;;                 (when (and (not (tramp-tramp-file-p file))
  ;;                         (file-directory-p file))
  ;;                   (setq collect-names (cons (concat "~" name) collect-names))
  ;;                   (insert (format "%s=\"%s\"\n" name (expand-file-name file) name)))))
  ;;         bookmark-alist)
  ;;       (insert ": " (mapconcat 'identity collect-names " ") "\n"))
  ;;     (let ((backup-inhibited t)) (save-buffer))
  ;;     (kill-buffer (current-buffer))))
  )

;; (use-package flyspell-mode
;;   :ensure nil
;;   :hook (text-mode . flyspell-mode)
;;   :init
;;   ;; use aspell as ispell backend
;;   (setq-default ispell-program-name "aspell")
;;   ;; use American English as ispell default dictionary
;;   (ispell-change-dictionary "american" t))

;; 让你的minibuffer变成垂直的
;; (fido-vertical-mode)

;; 自动保存
(auto-save-visited-mode)

;; 设置默认为左右分屏
(setq split-width-threshold 1)
(provide 'init-basic)
;;; init-basic.el ends here
