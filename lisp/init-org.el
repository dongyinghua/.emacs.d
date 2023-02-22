;;; init-org.el --- Initialize org configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Org configurations.
;;

;;; Code:

(require 'init-const)

;; 由于org 9.6.1版本存在问题，即在headline折叠状态下按回车键，新建行被插入到折叠（隐藏）区域内。
;; 暂时使用Emacs自带org。
(use-package org
  :pin melpa
  :ensure nil
  :defer 1
  :bind
  ("C-c a" . org-agenda)
  ("C-c x" . org-capture)
  ("C-c b" . org-switchb)
  ("C-c C-f a" . consul-org-agenda)
  ("C-c r" . org-refile)
  ("C-c 。" . org-time-stamp)
  :config
  ;; 使 org-mode 中的 timestamp 格式为英文
  (setq system-time-locale "C")

  ;; Add new template
  ;;(add-to-list 'org-structure-template-alist '("n" . "note"))

  ;;org-mode缩进
  ;; org-bars-mode 开启时会自动开启 org-indent-mode
  ;;(setq org-startup-indented t)

  ;; https://apple.stackexchange.com/questions/277928/error-auctex-cannot-find-a-working-tex-distribution-macos-sierra
  ;; (setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin/"))
  ;; (setq exec-path (append exec-path '("/Library/TeX/texbin/")))

  ;;使用 XeLaTeX 程序进行编译转换
  (setq org-latex-compiler "xelatex")
  (setq org-latex-pdf-process '("xelatex %f"))
  (add-to-list 'org-latex-default-packages-alist '("" "ctex" t ("xelatex")))

  ;; 解决org-mode中LaTeX数学公式中的中文渲染问题
  ;; https://q3yi.me/post/4_use_xelatex_instead_of_latex_in_org_preview_latex_process/
  (add-to-list 'org-preview-latex-process-alist
	       '(xdvsvgm :progams
			 ("xelatex" "dvisvgm")
			 :discription "xdv > svg"
			 :message "you need install the programs: xelatex and dvisvgm."
			 :image-input-type "xdv"
			 :image-output-type "svg"
			 :image-size-adjust (6.2 . 6) ; 调整 svg 的 size
			 :latex-compiler ("xelatex -interaction nonstopmode -no-pdf -output-directory %o %f")
			 :image-converter ("dvisvgm %f -n -b min -c %S -o %O")))

  (setq org-preview-latex-default-process 'xdvsvgm)


  ;; To speed up startup, don't put to init section
  (setq org-modules nil)     ;; Faster loading
  (setq org-startup-numerated t)

  (setq org-image-actual-width nil)
  ;;(org-display-inline-images t)

  ;;org-mode for GTD
  ;;todo dependencies
  ;;(setq alert-default-style 'notifications)

  ;;org-agenda
  (load-library "find-lisp")
  (setq org-agenda-files (find-lisp-find-files org-gtd-path "\.org$"))

  (setq org-refile-targets '((org-gtd-path-projects :maxlevel . 5)
                             (org-gtd-path-todos :maxlevel . 5)
                             (org-gtd-path-schedule :maxlevel . 5)
                             (org-gtd-path-inbox :maxlevel . 5)
			     ("~/Documents/Org/org-roam-directory/2022070322_科研笔记.org" :maxlevel . 5)
                             ))
  ;; (setq org-refile-targets '(("~/Documents/Org/GTD/Projects.org" :maxlevel . 5)
  ;;                            ("~/Documents/Org/GTD/TODOs.org" :maxlevel . 5)
  ;;                            ("~/Documents/Org/GTD/Schedule.org" :maxlevel . 5)
  ;;                            ("~/Documents/Org/GTD/Inbox.org" :maxlevel . 5)
  ;; 			     ("~/Documents/Org/org-roam-directory/2022070322_科研笔记.org" :maxlevel . 5)
  ;;                            ))

  ;; org-capture
  ;; 快捷键“C-c x”
  (setq org-capture-templates
	'(("i" "Inbox" entry
           (file+headline org-gtd-path-inbox "Tasks")
           "* TODO %?\n  %i\n"
           :empty-lines 0)
	  ("s" "Schedule" entry
           (file+headline org-gtd-path-schedule "Schedule")
           "* TODO %?\n  %i\n"
           :empty-lines 1)
	  ("t" "TODOs" entry
           (file+headline org-gtd-path-todos "TODOs")
           "* TODO %?\n  %i\n"
           :empty-lines 1)
	  ("p" "Projects" entry
           (file+headline org-gtd-path-projects "Projects")
           "* TODO %?\n  %i\n"
           :empty-lines 1)
	  )
	)

  ;; 设置任务流程
  ;; This is achieved by adding special markers ‘!’ (for a timestamp)
  ;; or ‘@’ (for a note with timestamp) in parentheses after each keyword.
  (setq org-todo-keywords
	'((sequence "DOING(i)" "TODO(t)" "HANGUP(h@/!)" "|" "DONE(d!)" "CANCEL(c@)"))
	org-todo-keyword-faces '(("TODO" . (:foreground "#F4606C" :weight blod))
				 ("DOING" . (:foreground "#19CAAD" :weight blod))
				 ("HANGUP" . (:foreground "#F4606C" :weight bold))
				 ("DONE" . (:foreground "#939391" :weight blod))
				 ("CANCEL" . (:background "gray" :foreground "black"))))

  (setq org-priority-faces '((?A . error)
                             (?B . warning)
                             (?C . success)))

  ;; need repeat task and properties
  (setq org-log-done t)
  (setq org-log-into-drawer t)

  (setq org-agenda-span 'day)
  ;;(add-hook org-capture-mode-hook 'evil-mode)

  (setq org-agenda-custom-commands
	'(("i" "重要且紧急的事" ;; 不显示没有加org-todo-keywords以及keyword是DONE的任务
           ((tags-todo "+PRIORITY=\"A\"")))
	  ;; ...other commands here
	  ))

  ;; org for beamer
  (eval-after-load "ox-latex"

    ;; update the list of LaTeX classes and associated header (encoding, etc.)
    ;; and structure
    '(add-to-list 'org-latex-classes
		  `("beamer"
		    ,(concat "\\documentclass[presentation]{beamer}\n"
			     "[DEFAULT-PACKAGES]"
			     "[PACKAGES]"
			     "[EXTRA]\n")
		    ("\\section{%s}" . "\\section*{%s}")
		    ("\\subsection{%s}" . "\\subsection*{%s}")
		    ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
  (setq org-latex-listings t)
  ;;     (setq org-emphasis-alist (quote (("*" bold "<b>" "</b>")
  ;;                                       ("/" italic "<i>" "</i>")
  ;;                                       ("_" underline "<span
  ;; style=\"text-decoration:underline;\">" "</span>")
  ;;                                       ("=" org-code "<code>" "</code>"
  ;;                                         verbatim)
  ;;                                       ("~" org-verbatim "<code>" "</code>"
  ;;                                         verbatim)
  ;;                                       ("+" (:strike-through t) "<del>" "</del>")
  ;;                                       ("@" org-warning "<b>" "</b>")))
  ;;       org-export-latex-emphasis-alist (quote
  ;;                                         (("*" "\\textbf{%s}" nil)
  ;;                                           ("/" "\\emph{%s}" nil)
  ;;                                           ("_" "\\underline{%s}" nil)
  ;;                                           ("+" "\\texttt{%s}" nil)
  ;;                                           ("=" "\\verb=%s=" nil)
  ;;                                           ("~" "\\verb~%s~" t)
  ;;                                           ("@" "\\alert{%s}" nil))))

  ;; 单独设置org标题字体大小，https://emacs-china.org/t/org/12869
  ;; 设置org标题1-8级的字体大小和颜色，颜色摘抄自monokai
  ;; 希望org-mode标题的字体大小和正文一致，设成1.0， 如果希望标题字体大一点可以设成1.2
  ;; org-mode正文height为200
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 215))))
   '(org-level-2 ((t (:inherit outline-2 :height 205))))
   '(org-level-3 ((t (:inherit outline-3 :height 200))))
   '(org-level-4 ((t (:inherit outline-4 :height 200))))
   ) ;; end custom-set-faces
  
  ) ; use-package org

;; (use-package org-superstar
;;   :if (and (display-graphic-p) (char-displayable-p ?◉))
;;   :hook (org-mode . org-superstar-mode)
;;   :config
;;   (setq org-superstar-headline-bullets-list '("▼")) ; no bullets
;;   (setq org-ellipsis " ▼ ")
;;   )

(use-package ox-beamer
  :ensure nil
  :defer t
  )

;; https://github.com/casouri/valign
;; 表格对齐
(use-package valign
  :ensure t
  :defer t
  :hook (org-mode . valign-mode)
  :config (setq valign-fancy-bar t))

;; (use-package org-contrib
;;   :pin nongnu
;;   :config
;;   ;; 对于需要重复完成的任务很有帮助
;;   (require 'org-checklist)
;;   )


;;（造轮子）定义了一个函数可以循环org-mode的emphasis-markers的可见性。
;; emphasis-markers就是org-mode轻语言的标记符号，比如说*、-等。
;; (defun org-cycling-emphasis-markers()
;;   (interactive)
;;   (if org-hide-emphasis-markers
;;     (setq org-hide-emphasis-markers nil)
;;     (setq org-hide-emphasis-markers t))
;;   (revert-buffer nil t nil))
;; (global-set-key (kbd "C-c c") 'org-cycling-emphasis-markers)

;; 但是有个更好的插件可以解决我这个需求：org-appear，以上就属于“造轮子”了
;; Github：https://github.com/awth13/org-appear
;; emacs-china：https://emacs-china.org/t/org-mode/16826
(use-package org-appear
  :ensure t
  :defer t
  :hook (org-mode . org-appear-mode)
  :config
  ;;使用evil-mode后，可以用以下代码来实现只在编辑模式下激活org-appear-mode
  ;;⚠️得先执行(setq org-hide-emphasis-markers t)，否则org-appear-autoemphasis会失效
  (setq org-hide-emphasis-markers t)
  (setq org-appear-autoemphasis t)
  (setq org-appear-autolinks t)
  (setq org-appear-trigger 'manual)
  (add-hook 'org-mode-hook (lambda ()
                             (add-hook 'evil-insert-state-entry-hook
				       'org-appear-manual-start
				       nil
				       t)
                             (add-hook 'evil-insert-state-exit-hook
				       'org-appear-manual-stop
				       nil
				       t)))) ; use-package org-appear

(use-package zotxt
  :ensure t
  :after org org-roam
  :defer t
  :hook
  (org . org-zotxt-mode)
  (org-roam-mode . org-zotxt-mode))

(use-package org-fragtog
  :ensure t
  :defer t
  ;; :hook
  ;; (org-mode . org-fragtog-mode)
  ;; (org-roam-mode . org-fragtog-mode)
  )

(use-package org-download
  :ensure t
  :defer t
  :hook
  (org-mode . org-download-enable)
  (org-roam-mode . org-download-enable)
  :init
  (setq-default org-download-heading-lvl 4)
  (setq-default org-download-image-dir "./images")
  (defun dummy-org-download-annotate-function (link)
    "")
  (setq org-download-annotate-function
	'dummy-org-download-annotate-function)
  )

(use-package org-re-reveal
  :ensure t
  :defer t
  ;;:hook (after-init . (lambda () (require 'org-re-reveal)))
  :init
  ;; reveal.js 的根目录
  (setq org-re-reveal-root "file:///Users/yinghuadong/reveal.js"))


(use-package ox-hugo
  :ensure t   ;Auto-install the package from Melpa
  :pin melpa  ;`package-archives' should already have ("melpa" . "https://melpa.org/packages/")
  :after ox)

(with-eval-after-load 'org-capture
  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
    (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
	   (fname (org-hugo-slug title)))
      (mapconcat #'identity
		 `(
		   ,(concat "* TODO " title)
		   ":PROPERTIES:"
		   ,(concat ":EXPORT_FILE_NAME: " fname)
		   ":END:"
		   "\n\n")          ;Place the cursor here finally
		 "\n")))

  (add-to-list 'org-capture-templates
	       '("h"                ;`org-capture' binding + h
		 "Hugo post"
		 entry
		 ;; It is assumed that below file is present in `org-directory'
		 ;; and that it has a "Blog Ideas" heading. It can even be a
		 ;; symlink pointing to the actual location of all-posts.org!
		 (file+headline "/Users/dragonli/Documents/Blogs/myblog/all-blog.org" "Blog Ideas")
		 (function org-hugo-new-subtree-post-capture-template))))

(provide 'init-org)
;;; init-org.el ends here
