;;; init.el --- A Fancy and Fast Emacs Configuration. -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:
;;
;; This file bootstraps the configuration, which is divided into
;; a number of other files.
;;


;;; Code:

;; (require 'cl)

(when (version< emacs-version "25.1")
  (error "This requires Emacs 25.1 and above!"))

(when (not (version< emacs-version "29.0"))
  (setq package-native-compile nil))

;; Speed up startup
(setq auto-mode-case-fold nil)

;; https://www.reddit.com/r/emacs/comments/xfhnzz/weird_errors_with_latest_build_of_emacs/
(when (eq system-type 'darwin) (customize-set-variable 'native-comp-driver-options '("-Wl,-w")))

;;如果不加这句代码的话，就会报错说找不到lisp中的.el文件
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Refer to https://github.com/purcell/emacs.d
;; 提高Emacs启动速度
(require 'init-benchmarking)

;; Set up $PATH. If you startup emacs in terminal, this is not necessary.
;; (require 'init-exec-path)

;; Package Management
(require 'init-packages)
(require 'init-hydra)

(require 'init-avy)
(require 'init-orderless)

;; Key Bindings
(require 'init-keybindings)

;; Emacs Basic Settings (Emacs内置功能的配置)
(require 'init-basic)
;; Emacs内置的outline-mode
(require 'init-outline)

(require 'init-dashboard) ; 要放到“init-ui”和“init-modeline”之前
(require 'init-ui)
(require 'init-doom-theme)
(require 'init-modeline)

;; ;; Font Setting
(require 'init-font)
(require 'init-clipboard)

;; (require 'init-ace-window)

(require 'init-persp)

;; (require 'init-evil)

(require 'init-completion)
(require 'init-company)
(require 'init-tools)
;; (require 'init-projectile)
;; (require 'init-git)
;; (require 'init-treemacs)

;; (require 'init-compile)
(require 'init-org)
(require 'init-org-gtd)
(require 'init-org-roam)

(require 'init-latex)
(require 'init-markdown)

;; (require 'init-yasnippet) ;;init-yasnippet需要在init-lsp-bridge前
;; (require 'init-lsp-bridge)
;; ;; (require 'init-eglot)

;; ;;Degug
;; ;;(require 'init-dap)
;; ;; 代码折叠
;; (require 'init-origami)
;; ;;Language
;; (require 'init-flycheck)
;; ;;(require 'init-c)
;; ;;(require 'init-java)
;; (require 'init-python)
;; (require 'init-matlab)
(require 'init-copilot)
(require 'init-kimi)
;; ;; (require 'init-pdf-tools)
;; ;;(require 'init-chatgpt-shell)

;; ;; 花里胡哨的配置
;; (require 'init-holo-layer)

(require 'init-eaf)

(provide 'init)
;;; init.el ends here
