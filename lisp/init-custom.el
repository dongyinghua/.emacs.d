;; init-custom.el --- Define customizations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Customization.
;;

;;; Code:

(defcustom dragonli-emacs-tools-file-path
  (expand-file-name "tools" user-emacs-directory)
  "This is the path of the thrid-party tools for Emacs."
  :group 'dragonli
  :type 'string)


;; Refer from https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-custom.el
(defcustom dragonli-icon (or (display-graphic-p) (daemonp))
  "Display icons or not."
  :group 'dragonli
  :type 'boolean)

;; @see https://github.com/melpa/melpa and https://elpa.emacs-china.org/.
(defcustom dragonli-package-archives-alist
  '((melpa    . (("gnu"    . "http://elpa.gnu.org/packages/")
                 ("nongnu" . "http://elpa.nongnu.org/nongnu/")
                 ("melpa"  . "http://melpa.org/packages/")))
    (emacs-cn . (("gnu"    . "http://1.15.88.122/gnu/")
                 ("nongnu" . "http://1.15.88.122/nongnu/")
                 ("melpa"  . "http://1.15.88.122/melpa/")))
    (bfsu     . (("gnu"    . "http://mirrors.bfsu.edu.cn/elpa/gnu/")
                 ("nongnu" . "http://mirrors.bfsu.edu.cn/elpa/nongnu/")
                 ("melpa"  . "http://mirrors.bfsu.edu.cn/elpa/melpa/")))
    (netease  . (("gnu"    . "http://mirrors.163.com/elpa/gnu/")
                 ("nongnu" . "http://mirrors.163.com/elpa/nongnu/")
                 ("melpa"  . "http://mirrors.163.com/elpa/melpa/")))
    (sjtu     . (("gnu"    . "http://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/gnu/")
                 ("nongnu" . "http://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/nongnu/")
                 ("melpa"  . "http://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/melpa/")))
    (tuna     . (("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                 ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                 ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
    (ustc     . (("gnu"    . "http://mirrors.ustc.edu.cn/elpa/gnu/")
                 ("nongnu" . "http://mirrors.ustc.edu.cn/elpa/nongnu/")
                 ("melpa"  . "http://mirrors.ustc.edu.cn/elpa/melpa/"))))
  "A list of the package archives."
  :group 'dragonli
  :type '(alist :key-type (symbol :tag "Archive group name")
                :value-type (alist :key-type (string :tag "Archive name")
                                   :value-type (string :tag "URL or directory name"))))

(defcustom dragonli-package-archives 'tuna
  "Set package archives from which to fetch."
  :group 'dragonli
  :set (lambda (symbol value)
         (set symbol value)
         (setq package-archives
               (or (alist-get value dragonli-package-archives-alist)
                   (error "Unknown package archives: `%s'" value))))
  :type `(choice ,@(mapcar
                    (lambda (item)
                      (let ((name (car item)))
                        (list 'const
                              :tag (capitalize (symbol-name name))
                              name)))
                    dragonli-package-archives-alist)))

(defcustom dragonli-dashboard (not (daemonp))
  "Display dashboard at startup or not.
If Non-nil, use dashboard, otherwise will restore previous session."
  :group 'dragonli
  :type 'boolean)

(defcustom my/completion-styles 'company
  "There are three choices for completion.
They are company, corfu and lsp-bridge.
For example, if the value is company, choicing the company-mode for completion."
  :group 'dragonli
  :type 'string)

(defcustom my-theme 'my-theme-name
  "The name of the theme."
  :group 'dragonli
  :type 'string)

(defcustom english-font 'english-font
  "English font."
  :group 'dragonli
  :type 'string)

(defcustom chinese-font 'chinese-font
  "Chinese font."
  :group 'dragonli
  :type 'string)

;; The prefix of the GTD files path.
(defcustom org-gtd-path "/Users/dragonli/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/"
  "The prefix of the GTD files."
  :group 'dragonli
  :type 'string)

;; The path of the GTD files.
(defcustom org-gtd-path-todos
  (expand-file-name "TODOs.org" org-gtd-path)
  "The path of the TODOs.org."
  :group 'dragonli
  :type 'string)

(defcustom org-gtd-path-projects
  (expand-file-name "Projects.org" org-gtd-path)
  "The path of the Projects.org."
  :group 'dragonli
  :type 'string)

(defcustom org-gtd-path-schedule
  (expand-file-name "Schedule.org" org-gtd-path)
  "The path of the Schedule.org."
  :group 'dragonli
  :type 'string)

(defcustom org-gtd-path-inbox
  (expand-file-name "Inbox.org" org-gtd-path)
  "The path of the Inbox.org."
  :group 'dragonli
  :type 'file)

(defcustom dragonli-select-company nil
  "If not-nil, using company."
  :group 'dragonli
  :type 'boolean)

;; the display format of the bold text in org mode.
;; 茶花红
(defface hi-red-custom
  '((t (:foreground "#ee3f4d")))
  "Face for hi lock: `茶花红'."
  :group 'dragonli)

;; 琥珀黄
(defface hi-yellow-custom
  '((t (:foreground "#feba07")))
  "Face for hi lock: `琥珀黄'."
  :group 'dragonli)

;; 竹绿
(defface hi-green-custom
  '((t (:foreground "#1ba784")))
  "Face for hi lock: `竹绿'."
  :group 'dragonli)

;; 青莲紫
(defface hi-purple-custom
  '((t (:foreground "#8b2671")))
  "Face for hi lock: `青莲紫'."
  :group 'dragonli)

;; 解决问题：
;; error in process filter: font-lock-default-fontify-region: Symbol’s value as variable is void: font-lock-reference-face
;; error in process filter: Symbol’s value as variable is void: font-lock-reference-face
;; 参考https://github.com/emacs-ess/ESS/issues/1254
;; 原因：font-lock-reference-face has been removed in Emacs 29 (it was previously obsolete). ess should use font-lock-constant-face instead.
(defvar font-lock-reference-face 'font-lock-reference-face
  "Face for references in an Rd file.")
(defface font-lock-reference-face
  '((default :inherit font-lock-keyword-face)
    (((class color) (background dark))
     (:foreground "lime green"))
    (((class color)) (background light) (:foreground "dark green")))
  "Face for references in an Rd file."
  :version "0.0")

(provide 'init-custom)
;;; init-custom.el ends here
