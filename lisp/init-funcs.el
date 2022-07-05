;;; init-funcs.el --- Define functions. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Define functions.
;;

;;; Code:

;; init-keybindings.el
;;快速打开配置文件
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun open-init-org()
  (interactive)
  (find-file "~/.emacs.d/lisp/init-org.el"))

;; init-basic.el
;;Emacs Server
;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs)
  )

;; init-packages.el
;; 增强 embark 和 consult，批量搜索替换大杀器
(defun embark-export-write()
  "Export the current vertico results to a writable buffer if possible.
Supports exporting consult-grep to wgrep, file to wdeired, and consult-location to occur-edit"
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
  (interactive)
  (setq truncate-lines nil)
  )

;; init-font.el
(defun s-font()
  (interactive)
  ;;Setting English Font
  (set-face-attribute
    'default nil :font "Source Code Pro 20" :weight 'normal)
  ;;Chinese Font
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
      charset
      (font-spec :family "冬青黑体简体中文 W6")))
  ;; tune rescale so that Chinese character width = 2 * English character width
  (setq face-font-rescale-alist '(("monospace" . 1.0) ("WenQuanYi" . 1.23)))
  )

(provide 'init-funcs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-funcs.el ends here
