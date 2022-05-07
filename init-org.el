(setq org-startup-indented t)
;;Different bullets. By default the list is set to ("◉" "○" "✸" "✿").
(setq org-bullets-bullet-list '("■" "◆" "▲" "▶"))

;;使用 XeLaTeX 程序进行编译转换
(setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f"
                              "xelatex -interaction nonstopmode %f"))

;;org-roam
;;必须要有init.el中加入(require 'org-roam-protocol)以及打开Emacs Server，否则网页抓取会出现问题
;;org-roam的网页抓取原理是利用 org-protocol 这样的外部程序和 Emacs 进行通信的机制
(setq org-roam-directory "~/Documents/Org/org-roam-directory")
(setq find-file-visit-truename t)
(org-roam-db-autosync-mode)

(setq org-roam-completion-everywhere t)
(require 'org-roam-protocol)
(setq org-roam-capture-templates
  '(
     ("s" "simple default" plain "%?"
       :target (file+head "%<%Y%m%d%H>_${slug}.org"
                 "#+STARTUP:\n#+title: ${title}\n\n")
       :unnarrowed t)
     ("d" "default" plain "%?"
       :target (file+head "%<%Y%m%d%H>_${slug}.org"
                 "#+STARTUP:\n#+title: ${title}\n#+roam_alias:\n#+roam_key:\n#+roam_tags:\n\n")
       :unnarrowed t)
     ("p" "Paper Note" plain "* FIRST PASS\n\n ** Category\n\n** Context\n\n** Correctness\n\n** Contribution\n\n** Clarity\n * SECOND PASS\n\n* * THIRD PASS"
       :target (file+head "%<%Y%m%d%H>_${slug}.org"
                 "#+STARTUP:\n#+title: ${title}\n#+roam_alias:\n#+roam_key:\n#+roam_tags:\n#+keywords:\n\n")
       :unnarrowed t
       )
     )
  )

;;网页抓取
(setq org-roam-capture-ref-templates
  '(
     ("a" "Annotation" plain
       "%U ${body}\n"
       :target (file+head "${slug}.org"
                 "#+title: ${title}\n#+roam_key: ${ref}\n#+roam_alias:\n#+roam_tags:\n\n")
       ;; :immediate-finish t
       :unnarrowed t
       )
     ("r" "ref" plain "* FIRST PASS\n\n** Category\n\n** Context\n\n** Correctness\n\n** Contribution\n\n** Clarity\n\n* SECOND PASS\n\n* THIRD PASS\n\n"
       :target (file+head "${slug}.org"
                 "#+title: ${title}\n#+roam_key: ${ref}\n#+roam_alias:\n#+roam_tags:\n\n")
       :unnarrowed t)
     )
  )

(use-package org-roam-ui
  :after org-roam
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
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

;;org-roam-dailies-directory
(setq org-roam-dailies-directory "~/Documents/Org/org-roam-directory/daily")

(setq org-roam-dailies-capture-templates
  '(("d" "default" entry
      "* %?"
      :target (file+head "%<%Y-%m-%d-%a>.org"
                "#+title: %<%Y-%m-%d-%a>\n"))
     ("w" "weekly report" entry
       "* %?"
       :target (file+head "%<%Y-%m-%d-%a>-周报.org"
                 "#+title: %<%Y-%m-%d-%a>-周报\n"))
     ("m" "monthly report" entry
       "* %?"
       :target (file+head "%<%Y-%m-%d-%a>-月报.org"
                 "#+title: %<%Y-%m-%d-%a>-月报\n"))
     ("M" "regular meeting" entry
       "* %?"
       :target (file+head "%<%Y-%m-%d-%a>-例会.org"
                 "#+title: %<%Y-%m-%d-%a>-例会\n"))))

;;org-mode for GTD
;;todo dependencies
(setq alert-default-style 'notifications)

;;org-agenda
(setq org-agenda-files '("~/Documents/Org/GTD/Inbox.org"
                          "~/Documents/Org/GTD/Projects.org"
                          "~/Documents/Org/GTD/Schedule.org"
                          "~/Documents/Org/GTD/TODOs.org"))
;; 这边就是为路径赋值
(defvar org-agenda-dir "" "gtd org files location")
(setq-default org-agenda-dir "~/Documents/Org/GTD/")
(setq org-agenda-file-inbox (expand-file-name "Inbox.org" org-agenda-dir))
(setq org-agenda-file-projects (expand-file-name "Projects.org" org-agenda-dir))
(setq org-agenda-file-TODOs (expand-file-name "TODOs.org" org-agenda-dir))
(setq org-agenda-files-schedule (expand-file-name "Schedule.org" org-agenda-dir))

;; 添加每次打开时可添加的任务类型
(setq org-capture-templates
  '(
     ("i" "Inbox" entry (file+headline org-agenda-file-inbox "Tasks")
       "* TODO %?\n  %i\n"
       :empty-lines 0)
     ("s" "Schedule" entry (file+headline org-agenda-file-schedule "Schedule")
       "* TODO %?\n  %i\n"
       :empty-lines 1)
     ("t" "TODOs" entry (file+headline org-agenda-file-TODOs "TODOs")
       "* TODO %?\n  %i\n"
       :empty-lines 1)
     )
  )

(setq org-refile-targets '(("~/Documents/Org/GTD/Projects.org" :maxlevel . 3)
                            ("~/Documents/Org/GTD/TODOs.org" :maxlevel . 3)
                            ("~/Documents/Org/GTD/Schedule.org" :maxlevel . 3)))

;; agenda 里面时间块彩色显示
;; From: https://emacs-china.org/t/org-agenda/8679/3
(defun ljg/org-agenda-time-grid-spacing ()
  "Set different line spacing w.r.t. time duration."
  (save-excursion
    (let* ((background (alist-get 'background-mode (frame-parameters)))
            (background-dark-p (string= background "dark"))
            (colors (list "#1ABC9C" "#2ECC71" "#3498DB" "#9966ff"))
            pos
            duration)
      (nconc colors colors)
      (goto-char (point-min))
      (while (setq pos (next-single-property-change (point) 'duration))
        (goto-char pos)
        (when (and (not (equal pos (point-at-eol)))
                (setq duration (org-get-at-bol 'duration)))
          (let ((line-height (if (< duration 30) 1.0 (+ 0.5 (/ duration 60))))
                 (ov (make-overlay (point-at-bol) (1+ (point-at-eol)))))
            (overlay-put ov 'face `(:background ,(car colors)
                                     :foreground
                                     ,(if background-dark-p "black" "white")))
            (setq colors (cdr colors))
            (overlay-put ov 'line-height line-height)
            (overlay-put ov 'line-spacing (1- line-height))))))))

(add-hook 'org-agenda-finalize-hook #'ljg/org-agenda-time-grid-spacing)
