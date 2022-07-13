;;; init-org.el --- Initialize org configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Org configurations.
;;

;;; Code:

(require 'init-const)

(use-package org
  :ensure t
  :pin melpa
  :bind
  ("C-c a" . org-agenda)
  ("C-c x" . org-capture)
  ("C-c b" . org-switchb)
  ("C-c C-f a" . consul-org-agenda)
  ("C-c r" . org-refile)
  :config
  ;;
  ;; Add new template
  (add-to-list 'org-structure-template-alist '("n" . "note"))

  ;;org-mode缩进
  (setq org-startup-indented t)

  ;; https://apple.stackexchange.com/questions/277928/error-auctex-cannot-find-a-working-tex-distribution-macos-sierra
  (setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin/"))
  (setq exec-path (append exec-path '("/Library/TeX/texbin/")))

  ;;使用 XeLaTeX 程序进行编译转换
  (setq org-latex-compiler "xelatex")

  ;; To speed up startup, don't put to init section
  (setq org-modules nil)     ;; Faster loading

  (setq org-image-actual-width nil)
  ;;(org-display-inline-images t)

  ;;org-mode for GTD
  ;;todo dependencies
  (setq alert-default-style 'notifications)

  ;;org-agenda
  (setq org-agenda-files '("~/Documents/Org/GTD/Inbox.org"
                            "~/Documents/Org/GTD/Projects.org"
                            "~/Documents/Org/GTD/Schedule.org"
                            "~/Documents/Org/GTD/TODOs.org"))

  (setq org-refile-targets '(("~/Documents/Org/GTD/Projects.org" :maxlevel . 3)
                              ("~/Documents/Org/GTD/TODOs.org" :maxlevel . 3)
                              ("~/Documents/Org/GTD/Schedule.org" :maxlevel . 3)
                              ("~/Documents/Org/GTD/Inbox.org" :maxlevel . 3)
                              ))

  ;; 添加每次打开时可添加的任务类型
  ;; 快捷键“C-c x”
  (setq org-capture-templates
    '(("i" "Inbox" entry
        (file+headline "~/Documents/Org/GTD/Inbox.org" "Tasks")
        "* TODO %?\n  %i\n"
        :empty-lines 0)
       ("s" "Schedule" entry
         (file+headline "~/Documents/Org/GTD/Schedule.org" "Schedule")
         "* TODO %?\n  %i\n"
         :empty-lines 1)
       ("t" "TODOs" entry
         ("~/Documents/Org/GTD/TODOs.org" "TODOs")
         "* TODO %?\n  %i\n"
         :empty-lines 1)
       ("p" "Projects" entry
         (file+headline "~/Documents/Org/GTD/Projects.org" "Projects")
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
                              ("CANCEL" . (:background "gray" :foreground "black")))
    )

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
  )

(use-package org-contrib
  :pin nongnu
  :config
  ;; 对于需要重复完成的任务很有帮助
  (require 'org-checklist)
  )

;; Prettify UI
  (if emacs/>=27p
      (use-package org-modern
        :hook ((org-mode . org-modern-mode)
               (org-agenda-finalize . org-modern-agenda)
               (org-modern-mode . (lambda ()
                                    "Adapt `org-modern-mode'."
                                    ;; Disable Prettify Symbols mode
                                    (setq prettify-symbols-alist nil)
                                    (prettify-symbols-mode -1)))))
    (progn
      (use-package org-superstar
        :if (and (display-graphic-p) (char-displayable-p ?◉))
        :hook (org-mode . org-superstar-mode)
        :init (setq org-superstar-headline-bullets-list '("◉""○""◈""◇""⁕")))
      (use-package org-fancy-priorities
        :diminish
        :hook (org-mode . org-fancy-priorities-mode)
        :init (setq org-fancy-priorities-list
                    (if (and (display-graphic-p) (char-displayable-p ?🅐))
                        '("🅐" "🅑" "🅒" "🅓")
                      '("HIGH" "MEDIUM" "LOW" "OPTIONAL"))))))


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
  :init
  (add-hook 'org-mode-hook 'org-appear-mode)
  :config
  ;;使用evil-mode后，可以用以下代码来实现只在编辑模式下激活org-appear-mode
  ;;⚠️得先执行(setq org-hide-emphasis-markers t)，否则org-appear-autoemphasis会失效
  (setq org-hide-emphasis-markers t)
  (setq org-appear-autoemphasis t)
  (setq org-appear-autolinks t)
  (setq org-appear-trigger 'manual)
  (add-hook 'org-mode-hook (lambda ()
                             (add-hook 'evil-insert-state-entry-hook
                               #'org-appear-manual-start
                               nil
                               t)
                             (add-hook 'evil-insert-state-exit-hook
                               #'org-appear-manual-stop
                               nil
                               t)
                             )
    )
  )





;; org-roam
(use-package org-roam
  :ensure t
  :config
  (setq org-roam-directory "~/Documents/Org/org-roam-directory")
  (setq find-file-visit-truename t)
  ;; 如果不激活org-roam-db-autosync-mode，就会导致org-roam-directory里的笔记不是最新的，
  ;; 也就是说新创建的笔记用“M-x org-roam-node-find”找不到
  (org-roam-db-autosync-mode)
  ;; (setq org-roam-completion-everywhere t)

  (setq org-roam-capture-templates
    '(("s" "simple default" plain "%?"
        :target (file+head "%<%Y%m%d%H>_${slug}.org"
                  "#+STARTUP:\n#+title: ${title}\n\n")
        :unnarrowed t)
       ("d" "default" plain "%?"
         :target (file+head "%<%Y%m%d%H>_${slug}.org"
                   "#+STARTUP:\n#+title: ${title}\n\n")
         :unnarrowed t)
       ("p" "Paper Note" plain "* FIRST PASS\n ** Category\n\n** Context\n\n** Correctness\n\n** Contribution\n\n** Clarity\n * SECOND PASS\n\n* * THIRD PASS"
         :target (file+head "%<%Y%m%d%H>_${slug}.org"
                   "#+STARTUP:\n#+title: ${title}\n\n")
         :unnarrowed t)
       )
    )

  ;;org-roam-dailies-directory
  (setq org-roam-dailies-directory "~/Documents/Org/org-roam-directory/daily")
  (setq org-roam-dailies-capture-templates
    '(("j" "journal" plain "* %?"
        :target (file+head "%<%Y-%m-%d-%a>-日志.org"
                  "#+title: %<%Y-%m-%d-%a>-日志\n")
        :unnarrowed t)
       ("w" "weekly report" plain "* %?"
         :target (file+head "%<%Y-%m-%d-%a>-周报.org"
                   "#+title: %<%Y-%m-%d-%a>-周报\n")
         :unnarrowed t)
       ("m" "monthly report" plain "* %?"
         :target (file+head "%<%Y-%m-%d-%a>-月报.org"
                   "#+title: %<%Y-%m-%d-%a>-月报\n")
         :unnarrowed t))
       ;;("M" "regular meeting" plain
       ;;  "* Mini Talk\n\n * Academic Report\n\n * Key Points"
       ;;  :target (file+head "%<%Y-%m-%d-%a>-例会.org"
       ;;            "#+title: %<%Y-%m-%d-%a>-例会\n")
       ;;  :unnarrowed t))
    )


  ;; 必须要在init.el中加入(require 'org-roam-protocol)以及打开Emacs Server，
  ;; 否则网页抓取会出现问题
  ;; org-roam的网页抓取原理是利用 org-protocol 这样的外部程序和 Emacs 进行通信的机制
  (require 'org-roam-protocol)

  ;;网页抓取
  (setq org-roam-capture-ref-templates
    '(("D" "Default" plain "\n"
        :target (file+head "${slug}.org" "#+title: ${title}\n\n")
        ;; :immediate-finish t
        :unnarrowed t)
       ("a" "Annotation" plain "%U ${body}\n"
         :target (file+head "${slug}.org" "#+title: ${title}\n\n")
         ;; :immediate-finish t
         :unnarrowed t)
       ("r" "Reference" plain
         "* FIRST PASS\n* Category\nWhat type of paper is this? A measurement paper? An analysis of an existing system? A description of a research prototype?\n\n** Context\nWhich other papers is it related to? Which theoretical bases were used to analyze the problem?\n\n** Correctness\nDo the assumptions appear to be valid?\n\n** Contribution\nWhat are the paper’s main contributions?\n\n** Clarity\nIs the paper well written?\n\n* SECOND PASS\n\n* THIRD PASS\n\n"
         :target (file+head "${slug}.org" "#+title: ${title}\n\n")
         :unnarrowed t)
       )
    )
  )


(use-package org-roam-ui
  :ensure t
  :after org-roam
  ;;normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;a hookable mode anymore, you're advised to pick something yourself
  ;;if you don't care about startup time, use
  ;;:hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
    org-roam-ui-follow t
    org-roam-ui-update-on-save t
    org-roam-ui-open-on-start t)
  )

(use-package org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :bind (:map org-mode-map
          (("C-c n a" . orb-note-actions))))



(use-package zotxt
  :ensure t
  :config
  (org-zotxt-mode))

(provide 'init-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
