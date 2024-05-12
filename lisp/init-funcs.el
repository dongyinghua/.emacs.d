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

(defun open-mac-learning-note ()
  "Open the note about learning Mac."
  (interactive)
  (find-file "~/Documents/Org/org-roam-directory/2022032322_my_macbook_manual.org"))

(defun open-org-roam-diary ()
  "Open the diary by org roam."
  (interactive)
  (find-file "~/Documents/Org/org-roam-directory/2022070322_org_roam_dailies_汇总.org"))

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

;; 从Emacs打开电脑文件
;; 注：子龙山人是在 Windows 平台上配置的，有些语句在 macOS 上不需要可以不用管
;; 子龙山人写的function一开始只能打开文件所在的文件夹，而不是该文件。
;; 原因在于他用了函数 file-name-directory，该函数是返回文件所在的文件夹的目录。
;; 把这个函数去除之后就可以了，所以我写了两个函数，一个是打开文件夹，一个是打开文件。
(defun consult-file-externally (file)
  "Open FILE externally using the default application of the system."
  (interactive "fOpen externally: ")
  (if (and (eq system-type 'windows-nt)
           (fboundp 'w32-shell-execute))
      (shell-command-to-string (encode-coding-string (replace-regexp-in-string "/" "\\\\" (format "explorer.app %s" (file-name-directory (expand-file-name file)))) 'gbk))
    (call-process (pcase system-type
                    ('darwin "open")
                    ('cygwin "cygstart")
                    (_ "xdg-open"))
		  nil 0 nil
		  ;; another function that open the directory where the `file' is.
		  ;; (file-name-directory (expand-file-name file))
		  ;; (file-name-directory FILENAME)
		  ;; Return the directory component in file name FILENAME. More details see "C-h f"
		  (expand-file-name file))
    )
  )

(defun consult-directory-externally (file)
  "Open the FILE's directory externally by system."
  (interactive "fOpen externally: ")
  (if (and (eq system-type 'windows-nt)
	   (fboundp 'w32-shell-execute))
      (shell-command-to-string (encode-coding-string (replace-regexp-in-string "/" "\\\\" (format "explorer.exe %s" (file-name-directory (expand-file-name file)))) 'gbk))
    (call-process (pcase system-type
		    ('darwin "open")
		    ('cygwin "cygstart")
		    (_ "xdg-open"))
		  nil 0 nil
		  (file-name-directory (expand-file-name file)))))

;; From Xah Lee http://xahlee.info/emacs/emacs/emacs_dired_open_file_in_ext_apps.html
(defun xah-open-in-external-app (&optional Fname)
  "Open the current file or Dired marked files in external app.
When called in `Emacs' Lisp, if `FNAME' is given, open that.

URL `http://xahlee.info/emacs/emacs/emacs_dired_open_file_in_ext_apps.html'
Version: 2019-11-04 2021-07-21 2022-08-19 2023-02-28 2023-03-10"
  (interactive)
  (let (xfileList xdoIt)
    (setq xfileList
          (if Fname
              (list Fname)
            (if (string-equal major-mode "dired-mode")
                (dired-get-marked-files)
              (list buffer-file-name))))
    (setq xdoIt (if (<= (length xfileList) 10) t (y-or-n-p "Open more than 10 files? ")))
    (when xdoIt
      (cond
       ((string-equal system-type "windows-nt")
        (let ((xoutBuf (get-buffer-create "*xah open in external app*"))
              (xcmdlist (list "PowerShell" "-Command" "Invoke-Item" "-LiteralPath")))
          (mapc
           (lambda (x)
             (message "%s" x)
             (apply 'start-process (append (list "xah open in external app" xoutBuf) xcmdlist (list (format "'%s'" (if (string-match "'" x) (replace-match "`'" t t x) x))) nil)))
           xfileList)

          (switch-to-buffer-other-window xoutBuf))
        ;; old code. calling shell. also have a bug if filename contain apostrophe
        ;; (mapc (lambda (xfpath) (shell-command (concat "PowerShell -Command \"Invoke-Item -LiteralPath\" " "'" (shell-quote-argument (expand-file-name xfpath)) "'"))) xfileList)
        )
       ((string-equal system-type "darwin")
        (mapc (lambda (xfpath) (shell-command (concat "open " (shell-quote-argument xfpath)))) xfileList))
       ((string-equal system-type "gnu/linux")
        (mapc (lambda (xfpath)
                (call-process shell-file-name nil nil nil
                              shell-command-switch
                              (format "%s %s"
                                      "xdg-open"
                                      (shell-quote-argument xfpath))))
              xfileList))
       ((string-equal system-type "berkeley-unix")
        (mapc (lambda (xfpath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" xfpath))) xfileList))))))

;;在所有由text-mode衍生出来的mode中禁用toggle-truncate-lines
(defun toggle-truncate-lines-off()
  "禁用 `toggle-truncate-lines'."
  (interactive)
  (setq truncate-lines nil)
  )

;; init-font.el
(defun set-font (english-font chinese-font)
  "Function for setting fonts.
The `ENGLISH-FONT' and `CHINESE-FONT' are respectively
the names of the English and Chinese font of Emacs."
  (interactive)
  ;;Setting English Font
  (set-face-attribute
   'default nil :family english-font :height 180 :weight 'normal)
  ;;Chinese Font
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
		      charset
		      (font-spec :family chinese-font)))
  ;; tune rescale so that Chinese character width = 2 * English character width
  (setq face-font-rescale-alist '((english-font . 1.0) (chinese-font . 1.23)))
  )

;; (defun s-font(english-font chinese-font)
;;   "Function for setting fonts."
;;   (interactive)
;;   ;;Setting English Font
;;   (set-face-attribute
;;    'default nil :font "Fira Code Retina 20" :weight 'normal)
;;   ;;Chinese Font
;;   (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;     (set-fontset-font (frame-parameter nil 'font)
;; 		      charset
;; 		      (font-spec :family "Sarasa Term SC Light")))
;;   ;; tune rescale so that Chinese character width = 2 * English character width
;;   (setq face-font-rescale-alist '(("Fira Code Retina" . 1.0) ("Sarasa Term SC Light" . 1.23)))
;;   )


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
  (and dragonli-icon
       (or (display-graphic-p) (daemonp))
       (or (featurep 'all-the-icons)
	   (require 'all-the-icons nil t))))

(defun dragonli-set-variable (variable value &optional no-save)
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
               (mapcar #'car dragonli-package-archives-alist)
               :preselect (symbol-name dragonli-package-archives)))))
  ;; Set option
  (dragonli-set-variable 'dragonli-package-archives archives no-save)

  ;; Refresh if need
  (and refresh (package-refresh-contents async))

  (message "Set package archives to `%s'" archives))
(defalias 'dragonli-set-package-archives #'set-package-archives)

;; Refer to https://emacs-china.org/t/elpa/11192
(defun dragonli-test-package-archives (&optional no-chart)
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
                     dragonli-package-archives-alist))
         (fastest (car (nth (cl-position (apply #'min durations) durations)
                            dragonli-package-archives-alist))))

    Display on chart
    (when (and (not no-chart)
               (require 'chart nil t)
               (require 'url nil t))
      (chart-bar-quickie
       'horizontal
       "Speed test for the ELPA mirrors"
       (mapcar (lambda (p) (symbol-name (car p))) dragonli-package-archives-alist)
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


;; (defun switch ()
;;   (interactive)
;;   (add-to-list 'load-path (expand-file-name "lsp-bridge" dragonli-emacs-tools-file-path))
;;   (setq company-mode nil)
;;   (require 'lsp-bridge)
;;   (lsp-bridge-mode)
;;   )

(defun enable-lsp-bridge ()
  "Enable the lsp-bridge."
  (interactive)
  (add-to-list 'load-path (expand-file-name "lsp-bridge" dragonli-emacs-tools-file-path))
  (require 'lsp-bridge)
  (lsp-bridge-mode)
  )

(defun dragonli-insert-setting-of-image ()
  "Insert the setting of image in org mode."
  (interactive)
  (insert "#+ATTR_ORG: :width 900"))

(defun org-bold-highlight ()
  "利用`highlight-regexp'高亮指定的正则表达式."
  (interactive)
  (highlight-regexp "[ \\t]\\(\\*\\(\\S-[^*]+\\S-\\|[^*]\\{1,2\\}\\)\\*\\)[ \\t\\n]*" 'hi-red-custom)
  ;;(highlight-regexp "(\\*\\(\\S-[^*]+\\S-\\|[^*]\\{1,2\\}\\)\\*\\)[ \\t\\n]*" 'hi-red-custom)
  )

(defun dragonli-insert-how-to-read-paper-first-pass ()
  "Insert the templet of the 5Cs which were proposed in 'how to read a paper'."
  (interactive)
  (insert "** First Pass\n*** Category\n\n*** Context\n\n*** Correctness\n\n*** Contribution\n\n*** Clarity\n\n")
  )

(defun dragonli-insert-setting-of-image ()
  "Insert the setting of image in org mode."
  (interactive)
  (setq size (read-from-minibuffer
	      (concat
	       (propertize "Image Size: " 'face '(bold default)))))
  (if (string= size "")
      ;; 默认值是700
      (insert (concat "#+ATTR_ORG: :width 700"))
    (insert (concat "#+ATTR_ORG: :width " size)))
  )

(defun dragonli-insert-in-line-formula-symbel-for-latex-formula ()
  "Insert `\(\)' at cursor point for latex formula in `org-mode' and `latex-mode'."
  (interactive)
  (insert "\\(\\)")
  (backward-char 2)
  )

(defun dragonli-insert-cite-for-latex-formula ()
  "Insert `\\upcite{}' at cursor point for latex formula in `org-mode' and `latex-mode'."
  (interactive)
  (insert "\\upcite{}")
  (backward-char 1)
  )


(defun dragonli-save-file()
  "Save the current buffer."
  (interactive)
  (editorconfig-format-buffer)
  (save-buffer)
  )

(defun my/check-projectile-cache-file ()
  " 检查 projectile-cache-file 里的工程目录和文件是否存在， 如果不存在则删掉 "
  (interactive)
  (projectile-serialize-cache)   ;; 把 cache 写到文件里
  (let* ((new-cache (with-temp-buffer
                      (insert-file projectile-cache-file)
                      (goto-char (point-min))
                      (while (re-search-forward ")\\s-+\"\\(.*?\\)\"\\s-+(" nil t)
                        ;; (message "prj : %S" (match-string 1))
                        (let* ((prj-name (match-string 1)))
                          (unless (file-exists-p prj-name)
                            (message "clearing not found prj : %S" prj-name)
                            (delete-region (- (point) (1- (length (match-string 0)))) (1- (point)))
                            (backward-char 1)
                            (when (re-search-forward "(\\(.*?\\))" nil nil)
                              ;; (message "files : %s" (match-string 1))
                              (delete-region (- (point) (length (match-string 0))) (point))
                              ))))
                      (buffer-string))))
    (f-write-text new-cache 'utf-8 projectile-cache-file)
    ;; 从文件更新到 cache 中, 这样在关闭emacs 后才不会使改动丢失
    (setq projectile-projects-cache
          (or (projectile-unserialize projectile-cache-file)
              (make-hash-table :test 'equal)))))

(provide 'init-funcs)
;;; init-funcs.el ends here
