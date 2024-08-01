;;; init-org-gtd.el --- Initialize org gtd configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Org gtd configurations.
;;

;;; Code:

;; (use-package org-gtd
;;   :ensure nil
;;   ) ;; use-package ends here

;; org-mode for GTD
;; todo dependencies
;;(setq alert-default-style 'notifications)

;; org-agenda
(load-library "find-lisp")
(setq org-agenda-files (find-lisp-find-files org-gtd-path "\.org$"))

(setq-default org-refile-targets '((org-gtd-path-projects :maxlevel . 5)
				   (org-gtd-path-todos :maxlevel . 5)
				   (org-gtd-path-schedule :maxlevel . 5)
				   (org-gtd-path-inbox :maxlevel . 5)
				   ))
;; (setq org-refile-targets '(("~/Documents/Org/GTD/Projects.org" :maxlevel . 5)
;;                            ("~/Documents/Org/GTD/TODOs.org" :maxlevel . 5)
;;                            ("~/Documents/Org/GTD/Schedule.org" :maxlevel . 5)
;;                            ("~/Documents/Org/GTD/Inbox.org" :maxlevel . 5)
;; 			     ("~/Documents/Org/org-roam-directory/2022070322_科研笔记.org" :maxlevel . 5)
;;                            ))
(setq-default org-deadline-warning-days 30)

;; org-capture
;; 快捷键“C-c x”
(setq-default org-capture-templates
	      '(("i" "Inbox" entry
		 (file+headline org-gtd-path-inbox "Tasks")
		 "* TODO %?\n  %i\n"
		 :empty-lines 0)
		("s" "Schedule" entry
		 (file+headline org-gtd-path-schedule "Schedules")
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

(setq-default org-agenda-span 'day)
;;(add-hook org-capture-mode-hook 'evil-mode)

(setq-default org-agenda-custom-commands
	      '(("i" "重要且紧急的事" ;; 不显示没有加org-todo-keywords以及keyword是DONE的任务
		 ((tags-todo "+PRIORITY=\"A\"")))
		;; ...other commands here
		))

(setq org-agenda-sorting-strategy
      '((agenda time-up habit-down priority-down category-up)
	(todo   priority-down category-up)
	(tags   priority-down category-up)
	(search category-up)))

;; appt
(use-package appt
  :ensure nil
  :config
  ;; 每小时同步一次appt,并且现在就开始同步
  (run-at-time nil 3600 'org-agenda-to-appt)
  ;; 更新agenda时，同步appt
  (add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)
  ;; 激活提醒
  (appt-activate 1)
  ;; 提前10分钟提醒
  (setq appt-message-warning-time 5)
  ;; 每5分钟提醒一次
  (setq appt-display-interval 5)
  ;; (appt-disp-window min-to-appt current-time appt-msg)
  )

;; 取消log
(setq org-log-done nil)

(provide 'init-org-gtd)
;;; init-org-gtd.el ends here
