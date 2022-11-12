;;; init-funcs.el --- Define functions. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Define functions.
;;

;;; Code:

(require 'init-custom)

;; init-keybindings.el
;;快速打开配置文件
(defun open-init-file()
  "Open `init.el'."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun open-init-org()
  "Open `init-org.el'."
  (interactive)
  (find-file "~/.emacs.d/lisp/init-org.el"))

(defun open-emacs-learning-note ()
  "Open the note about learning Emacs."
  (interactive)
  (find-file "~/Documents/Org/org-roam-directory/2022021709_emacs_learning_note.org"))

(defun open-org-learning-note ()
  "Open the note about learning Org."
  (interactive)
  (find-file "~/Documents/Org/org-roam-directory/2022021709_org_learning_note.org"))

;; init-basic.el
;;Emacs Server
;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (kill-emacs)
  )

;; init-packages.el
;; 增强 embark 和 consult，批量搜索替换大杀器
(defun embark-export-write ()
  "Export the current vertico results to a writable buffer if possible.
Supports exporting consult-grep to wgrep,
file to wdeired, and consult-location to occur-edit"
  (interactive)
  (require 'embark)
  (require 'wgrep)
  (pcase-let ((`(,type . ,candidates)
                (run-hook-with-args-until-success 'embark-candidate-collectors)))
    (pcase type
      ('consult-grep (let ((embark-after-export-hook #'wgrep-change-to-wgrep-mode))
                       (embark-export)))
      ('file (let ((embark-after-export-hook #'wdired-change-to-wdired-mode))
               (embark-export)))
      ('consult-location (let ((embark-after-export-hook #'occur-edit-mode))
                           (embark-export)))
      (x (user-error "embark category %S doesn't support writable export" x)))
    )
  )

;;从Emacs打开电脑文件
;;注：子龙山人是在Windows平台上配置的，有些语句在macOS上不需要可以不用管
(defun consult-directory-externally (file)
  "Open FILE externally using the default application of the system."
  (interactive "fOpen externally: ")
  (if (and (eq system-type 'windows-nt)
        (fboundp 'w32-shell-execute))
    (shell-command-to-string (encode-coding-string (replace-regexp-in-string "/" "\\\\"(format "explorer.exe %s" (file-name-directory (expand-file-name file)))) 'gbk))
    (call-process (pcase system-type
                    ('darwin "open")
                    ('cygwin "cygstart")
                    (_ "xdg-open"))
      nil 0 nil
      (file-name-directory (expand-file-name file)))
    )
  )

;;在所有由text-mode衍生出来的mode中禁用toggle-truncate-lines
(defun toggle-truncate-lines-off()
  "禁用 `toggle-truncate-lines'."
  (interactive)
  (setq truncate-lines nil)
  )

;; init-font.el
(defun s-font()
  "Setting fonts."
  (interactive)
  ;;Setting English Font
  (set-face-attribute
    'default nil :font "Fira Code Retina 20" :weight 'normal)
  ;;Chinese Font
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
      charset
      (font-spec :family "冬青黑体简体中文 W6")))
  ;; tune rescale so that Chinese character width = 2 * English character width
  (setq face-font-rescale-alist '(("Fira Code Retina" . 1.0) ("冬青黑体简体中文 W6" . 1.23)))
  )

(defun avy-action-embark (pt)
  (unwind-protect
    (save-excursion
      (goto-char pt)
      (embark-act))
    (select-window
      (cdr (ring-ref avy-ring 0))))
  t)


;; Refer to https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-funcs.el
(defun icon-displayable-p ()
  "Return non-nil if icons are displayable."
  (and centaur-icon
    (or (display-graphic-p) (daemonp))
    (or (featurep 'all-the-icons)
      (require 'all-the-icons nil t))))

(defun centaur-set-variable (variable value &optional no-save)
  "Set the VARIABLE to VALUE, and return VALUE.
Save to `custom-file' if NO-SAVE is nil."
  (customize-set-variable variable value)
  (when (and (not no-save)
          (file-writable-p custom-file))
    (with-temp-buffer
      (insert-file-contents custom-file)
      (goto-char (point-min))
      (while (re-search-forward
               (format "^[\t ]*[;]*[\t ]*(setq %s .*)" variable)
               nil t)
        (replace-match (format "(setq %s '%s)" variable value) nil nil))
      (write-region nil nil custom-file)
      (message "Saved %s (%s) to %s" variable value custom-file))))

;; Pakcage repository (ELPA)
(defun set-package-archives (archives &optional refresh async no-save)
  "Set the package ARCHIVES (ELPA).
REFRESH is non-nil, will refresh archive contents.
ASYNC specifies whether to perform the downloads in the background.
Save to `custom-file' if NO-SAVE is nil."
  (interactive
    (list
      (intern
        (ivy-read "Select package archives: "
          (mapcar #'car centaur-package-archives-alist)
          :preselect (symbol-name centaur-package-archives)))))
  ;; Set option
  (centaur-set-variable 'centaur-package-archives archives no-save)

  ;; Refresh if need
  (and refresh (package-refresh-contents async))

  (message "Set package archives to `%s'" archives))
(defalias 'centaur-set-package-archives #'set-package-archives)

;; Refer to https://emacs-china.org/t/elpa/11192
(defun centaur-test-package-archives (&optional no-chart)
  "Test connection speed of all package archives and display on chart.
Not displaying the chart if NO-CHART is non-nil.
Return the fastest package archive."
  (interactive)

  (let* ((durations (mapcar
                      (lambda (pair)
                        (let ((url (concat (cdr (nth 2 (cdr pair)))
                                     "archive-contents"))
                               (start (current-time)))
                          (message "Fetching %s..." url)
                          (ignore-errors
                            (url-copy-file url null-device t))
                          (float-time (time-subtract (current-time) start))))
                      centaur-package-archives-alist))
          (fastest (car (nth (cl-position (apply #'min durations) durations)
                          centaur-package-archives-alist))))

    Display on chart
    (when (and (not no-chart)
            (require 'chart nil t)
            (require 'url nil t))
      (chart-bar-quickie
        'horizontal
        "Speed test for the ELPA mirrors"
        (mapcar (lambda (p) (symbol-name (car p))) centaur-package-archives-alist)
        "ELPA"
        (mapcar (lambda (d) (* 1e3 d)) durations) "ms"))

    (message "`%s' is the fastest package archive" fastest)

    ;; Return the fastest
    fastest))

(defun childframe-workable-p ()
  "Whether childframe is workable."
  (or (not (or noninteractive
             emacs-basic-display
             (not (display-graphic-p))))
    (daemonp)))

;; Functions for org mode.

;; 去除折叠的标题后的省略号，来自 org-bars 的文档。
;; 用法 (use-package :hook (org-mode . org-no-ellipsis-in-headlines))
(defun org-no-ellipsis-in-headlines ()
  "Remove use of ellipsis in headlines.
See `buffer-invisibility-spec'."
  (remove-from-invisibility-spec '(outline . t))
  (add-to-invisibility-spec 'outline))


;;按f11让Emacs进入全屏显示
;;参考： http://www.emacswiki.org/cgi-bin/wiki/FullScreen
(defun fullscreen ()
  "按 f11 让 Emacs 进入全屏显示."
  (interactive)
  (set-frame-parameter nil 'fullscreen
    (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

(provide 'init-funcs)
;;; init-funcs.el ends here
