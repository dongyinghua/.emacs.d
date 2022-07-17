;;; init-dashboard.el --- Initialize dashboard configurations.  -*- lexical-binding: t -*-

;;; Commentary
;;
;; Dashboard configurations.
;;

;;; Code:

(require 'init-custom)
(require 'init-funcs)

(when centaur-dashboard
  (use-package dashboard
    ;;:diminish dashboard-mode
    :functions (all-the-icons-faicon
                 all-the-icons-material
                 winner-undo
                 widget-forward)
    :custom-face (dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
    :pretty-hydra
    ((:title
       (pretty-hydra-title "Dashboard" 'material "dashboard" :height 1.2 :v-adjust -0.2)
       :color pink :quit-key "q")
      ("Navigator"
        (("U" update-config-and-packages "update" :exit t)
          ("H" browse-homepage "homepage" :exit t)
          ("R" restore-previous-session "recover session" :exit t)
          ("L" restore-session "list sessions" :exit t)
          ("S" open-custom-file "settings" :exit t))
        "Section"
        (("}" dashboard-next-section "next")
          ("{" dashboard-previous-section "previous")
          ("r" dashboard-goto-recent-files "recent files")
          ("m" dashboard-goto-bookmarks "bookmarks")
          ("p" dashboard-goto-projects "projects"))
        "Item"
        (("RET" widget-button-press "open" :exit t)
          ("<tab>" widget-forward "next")
          ("C-i" widget-forward "next")
          ("<backtab>" widget-backward "previous")
          ("C-n" next-line "next line")
          ("C-p" previous-line "previous  line"))
        "Misc"
        (("<f2>" open-dashboard "open" :exit t)
          ("g" dashboard-refresh-buffer "refresh" :exit t)
          ("Q" quit-dashboard "quit" :exit t))))
    :bind (("<f2>" . open-dashboard)
            :map dashboard-mode-map
            ("H" . browse-homepage)
            ("R" . restore-previous-session)
            ("L" . restore-session)
            ("S" . open-custom-file)
            ("U" . update-config-and-packages)
            ("q" . quit-dashboard)
            ("h" . dashboard-hydra/body)
            ("?" . dashboard-hydra/body))
    :hook (dashboard-mode . (lambda () (setq-local frame-title-format nil)))
    :init
    (dashboard-setup-startup-hook)
    (setq dashboard-banner-logo-title "Welcome to Emacs!") ;; 个性签名，随读者喜好设置
    ;; (setq dashboard-projects-backend 'projectile) ;; 读者可以暂时注释掉这一行，等安装了 projectile 后再使用

    ;; Set the banner
    (setq dashboard-startup-banner "~/.emacs.d/emacs-dragon.png")
    ;;(setq dashboard-startup-banner [VALUE])
    ;; Value can be
    ;; 'official which displays the official emacs logo
    ;; 'logo which displays an alternative emacs logo
    ;; 1, 2 or 3 which displays one of the text banners
    ;; "path/to/your/image.gif", "path/to/your/image.png" or "path/to/your/text.txt" which displays whatever gif/image/text you would prefer

    (setq dashboard-page-separator "\n\f\n")

    ;; Content is not centered by default. To center, set
    (setq dashboard-center-content t)

    ;; To disable shortcut "jump" indicators for each section, set
    (setq dashboard-show-shortcuts nil)

    (setq dashboard-items '((recents  . 10)   ; 显示多少个最近文件
                             (agenda . 5)
                             (bookmarks . 5)))  ; 显示多少个最近书签

    ;; To add icons to the widget headings and their items:
    (setq dashboard-set-heading-icons t)
    (setq dashboard-set-file-icons t)
    ;; To modify heading icons with another icon from all-the-icons octicons:
    (setq dashboard-heading-icons '((recents   . "history")
                                     (bookmarks . "bookmark")
                                     (agenda    . "calendar")))
    ;; (projects  . "briefcase")
    ;; (registers . "database")))

    (setq dashboard-set-footer t
      dashboard-footer (format "Powered by Yinghua Dong, %s" (format-time-string "%Y"))
      dashboard-footer-icon (cond ((icon-displayable-p)
                                    (all-the-icons-faicon "heart"
                                      :height 1.1
                                      :v-adjust -0.05
                                      :face 'error))
                              ((char-displayable-p ?🧡) "🧡 ")
                              (t (propertize ">" 'face 'dashboard-footer))))

    ;; To show navigator below the banner:
    (setq dashboard-set-navigator t)
    ;; To customize the buttons of the navigator like this:
    ;; Format: "(icon title help action face prefix suffix)"
    (setq dashboard-navigator-buttons
      `(((,(when (icon-displayable-p)
             (all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0))
           "Homepage" "Browse homepage"
           (lambda (&rest _) (browse-url centaur-homepage)))
          (,(when (icon-displayable-p)
              (all-the-icons-material "restore" :height 1.35 :v-adjust -0.24))
            "Restore" "Restore previous session"
            (lambda (&rest _) (restore-previous-session)))
          (,(when (icon-displayable-p)
              (all-the-icons-octicon "tools" :height 1.0 :v-adjust 0.0))
            "Settings" "Open custom file"
            (lambda (&rest _) (find-file custom-file)))
          (,(when (icon-displayable-p)
              (all-the-icons-material "update" :height 1.35 :v-adjust -0.24))
            "Update" "Update Centaur Emacs"
            (lambda (&rest _) (centaur-update)))
          (,(if (icon-displayable-p)
              (all-the-icons-faicon "question" :height 1.2 :v-adjust -0.1)
              "?")
            "" "Help (?/h)"
            (lambda (&rest _) (dashboard-hydra/body))))))

    ;; To show info about the packages loaded and the init time:
    (setq dashboard-set-init-info t)

    :config
    ;; WORKAROUND: fix differnct background color of the banner image.
    ;; @see https://github.com/emacs-dashboard/emacs-dashboard/issues/203
    (defun my-dashboard-insert-image-banner (banner)
      "Display an image BANNER."
      (when (file-exists-p banner)
        (let* ((title dashboard-banner-logo-title)
                (spec (create-image banner))
                (size (image-size spec))
                (width (car size))
                (left-margin (max 0 (floor (- dashboard-banner-length width) 2))))
          (goto-char (point-min))
          (insert "\n")
          (insert (make-string left-margin ?\ ))
          (insert-image spec)
          (insert "\n\n")
          (when title
            (dashboard-center-line title)
            (insert (format "%s\n\n" (propertize title 'face 'dashboard-banner-logo-title)))))))
    (advice-add #'dashboard-insert-image-banner :override #'my-dashboard-insert-image-banner)

    (defun restore-previous-session ()
      "Restore the previous session."
      (interactive)
      (when (bound-and-true-p persp-mode)
        (restore-session persp-auto-save-fname)))

    (defun restore-session (fname)
      "Restore the specified session."
      (interactive (list (read-file-name "Load perspectives from a file: "
                           persp-save-dir)))
      (when (bound-and-true-p persp-mode)
        (message "Restoring session...")
        (quit-window t)
        (condition-case-unless-debug err
          (persp-load-state-from-file fname)
          (error "Error: Unable to restore session -- %s" err))
        (message "Restoring session...done")))

    (defun dashboard-goto-recent-files ()
      "Go to recent files."
      (interactive)
      (let ((func (local-key-binding "r")))
        (and func (funcall func))))

    (defun dashboard-goto-projects ()
      "Go to projects."
      (interactive)
      (let ((func (local-key-binding "p")))
        (and func (funcall func))))

    (defun dashboard-goto-bookmarks ()
      "Go to bookmarks."
      (interactive)
      (let ((func (local-key-binding "m")))
        (and func (funcall func))))

    (defvar dashboard-recover-layout-p nil
      "Wether recovers the layout.")

    (defun open-dashboard ()
      "Open the *dashboard* buffer and jump to the first widget."
      (interactive)
      ;; Check if need to recover layout
      (if (length> (window-list-1)
            ;; exclude `treemacs' window
            (if (and (fboundp 'treemacs-current-visibility)
                  (eq (treemacs-current-visibility) 'visible))
              2
              1))
        (setq dashboard-recover-layout-p t))

      ;; Display dashboard in maximized window
      (delete-other-windows)

      ;; Refresh dashboard buffer
      (dashboard-refresh-buffer)

      ;; Jump to the first section
      (dashboard-goto-recent-files))

    (defun quit-dashboard ()
      "Quit dashboard window."
      (interactive)
      (quit-window t)
      (and dashboard-recover-layout-p
        (and (bound-and-true-p winner-mode) (winner-undo))
        (setq dashboard-recover-layout-p nil)))))

(provide 'init-dashboard)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dashboard.el ends here
