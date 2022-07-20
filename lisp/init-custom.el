;; init-custom.el --- Define customizations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Customization.
;;

;;; Code:

(defcustom dragonli-emacs-tools-file-path
  (expand-file-name "tools" user-emacs-directory)
  "This is the path of the thrid-party tools for emacs"
  :group 'dragonli
  :type 'string)


;; Refer from https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-custom.el
(defcustom centaur-icon (or (display-graphic-p) (daemonp))
  "Display icons or not."
  :group 'centaur
  :type 'boolean)

;; @see https://github.com/melpa/melpa and https://elpa.emacs-china.org/.
(defcustom centaur-package-archives-alist
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
  :group 'centaur
  :type '(alist :key-type (symbol :tag "Archive group name")
                :value-type (alist :key-type (string :tag "Archive name")
                                   :value-type (string :tag "URL or directory name"))))

(defcustom centaur-package-archives 'bfsu
  "Set package archives from which to fetch."
  :group 'centaur
  :set (lambda (symbol value)
         (set symbol value)
         (setq package-archives
               (or (alist-get value centaur-package-archives-alist)
                   (error "Unknown package archives: `%s'" value))))
  :type `(choice ,@(mapcar
                    (lambda (item)
                      (let ((name (car item)))
                        (list 'const
                              :tag (capitalize (symbol-name name))
                              name)))
                     centaur-package-archives-alist)))

(defcustom centaur-dashboard (not (daemonp))
  "Display dashboard at startup or not.
If Non-nil, use dashboard, otherwise will restore previous session."
  :group 'centaur
  :type 'boolean)



(defcustom is-company t
  "If not-nil, using company"
  :group 'dragonli
  :type 'boolean)

(defcustom my/completion-styles 'company
  "There are three choices for completion. They are company, corfu and lsp-bridge. For example, if the value is company, choicing the company-mode for completion."
  :group 'dragonli
  :type 'string)

(provide 'init-custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-custom.el ends here
