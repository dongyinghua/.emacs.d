;;; init-org.el --- Initialize org-roam configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Org-roam configurations.
;;

;;; Code:

;; org-roam
(use-package org-roam
  :ensure t
  :defer t
  :bind ("C-c o f" . org-roam-node-find)
  :config
  (setq org-roam-directory "~/Documents/Org/org-roam-directory")

  (setq find-file-visit-truename t)
  ;; 如果不激活org-roam-db-autosync-mode，就会导致org-roam-directory里的笔记不是最新的，
  ;; 也就是说新创建的笔记用“M-x org-roam-node-find”找不到
  (org-roam-db-autosync-mode)
  (setq org-roam-completion-everywhere t)

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
           :unnarrowed t)))

  ;;org-roam-dailies-directory
  (setq-default org-roam-dailies-directory "~/Documents/Org/org-roam-directory/diary")
  (setq-default org-roam-dailies-capture-templates
		'(("j" "journal" plain "* %?"
		   :target (file+head "%<%Y-%m-%d-%a>-Diary.org"
				      "#+title: %<%Y-%m-%d-%a>-Diary\n")
		   :unnarrowed t)
		  ("w" "weekly report" plain
		   "* 本周工作总结\n\n * 下周工作安排"
		   :target (file+head "%<%Y-%m-%d-%a>-Weekly.org"
				      "#+title: %<%Y-%m-%d-%a>-Weekly\n")
		   :unnarrowed t)
		  ("m" "monthly report" plain
		   "* 本月工作总结\n\n * 下月工作安排"
		   :target (file+head "%<%Y-%m-%d-%a>-Monthly.org"
				      "#+title: %<%Y-%m-%d-%a>-Monthly\n")
		   :unnarrowed t))
		)

  ;; 必须要在init.el中加入(require 'org-roam-protocol)以及打开Emacs Server，
  ;; 否则网页抓取会出现问题
  ;; org-roam的网页抓取原理是利用 org-protocol 这样的外部程序和 Emacs 进行通信的机制
  (require 'org-roam-protocol)

  ;;网页抓取
  (setq-default org-roam-capture-ref-templates
		'(("D" "Default" plain "\n"
		   :target (file+head "${slug}.org" "#+title: ${title}\n\n")
		   ;; :immediate-finish t
		   :unnarrowed t)
		  ("a" "Annotation" plain "%U ${body}\n"
		   :target (file+head "${slug}.org" "#+title: ${title}\n\n")
		   ;; :immediate-finish t
		   :unnarrowed t)
		  ("r" "Reference" plain
		   "* FIRST PASS\n** Category\nWhat type of paper is this? A measurement paper? An analysis of an existing system? A description of a research prototype?\n\n** Context\nWhich other papers is it related to? Which theoretical bases were used to analyze the problem?\n\n** Correctness\nDo the assumptions appear to be valid?\n\n** Contribution\nWhat are the paper’s main contributions?\n\n** Clarity\nIs the paper well written?\n\n* SECOND PASS\n\n* THIRD PASS\n\n"
		   :target (file+head "${slug}.org" "#+title: ${title}\n\n")
		   :unnarrowed t))
		)
  )


(use-package org-roam-ui
  :ensure t
  :defer t
  :bind ("C-c o i" . org-roam-ui-open)
  ;;normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;a hookable mode anymore, you're advised to pick something yourself
  ;;if you don't care about startup time, use
  ;;:hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
	org-roam-ui-follow t
	org-roam-ui-update-on-save t
	org-roam-ui-open-on-start t))

(use-package org-roam-bibtex
  :after org-roam
  :defer t
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :bind (:map org-mode-map
              (("C-c n a" . orb-note-actions))))

(provide 'init-org-roam)
;;; init-org-roam.el ends here
