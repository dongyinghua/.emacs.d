;;; init-company.el --- Initialize company configurations. -*- lexical-binding: t -*-

;;; Commentary
;;
;; Auto-completion configurations.
;;

;;; Code:

(require 'init-funcs)
(require 'init-custom)

(use-package all-the-icons
  :ensure t)

(setq my/completion-styles "company")

;;company
;;放弃使用auto-complete，转用company
;;“C-n”和“C-p”来在补全提示栏中选择补全项
(when (string= my/completion-styles "company")
  (use-package company
    :hook (after-init . global-company-mode)
    :init
    (setq company-minimum-prefix-length 2)
    (setq company-idle-delay 0.0)
    (setq company-show-numbers t) ; 给选项编号 (按快捷键 M-1、M-2 等等来进行选择).
    (setq company-selection-wrap-around t)
    (setq company-transformers '(company-sort-by-occurrence)) ; 根据选择的频率进行排序，读者如果不喜欢可以去掉

    :config
    ;;tab键补全
    (setq tab-always-indent 'complete)

    ;; company-tabnine AI 自动补全
    (use-package company-tabnine
      :init (add-to-list 'company-backends #'company-tabnine)
      :config
      ;; workaround for company-transformers
      (setq company-tabnine--disable-next-transform nil)
      (defun my-company--transform-candidates (func &rest args)
        (if (not company-tabnine--disable-next-transform)
          (apply func args)
          (setq company-tabnine--disable-next-transform nil)
          (car args)))

      (defun my-company-tabnine (func &rest args)
        (when (eq (car args) 'candidates)
          (setq company-tabnine--disable-next-transform t))
        (apply func args))

      (advice-add #'company--transform-candidates :around #'my-company--transform-candidates)
      (advice-add #'company-tabnine :around #'my-company-tabnine)))

  ;; 图形界面
  (use-package company-box
    :hook (company-mode . company-box-mode))

  ;; org-bars 的文档中说到，如果同时使用 org-bars 和 company-mode，就需要使用company-posframe
  (use-package company-posframe
    :hook (company-mode . company-posframe-mode)
    :config (setq company-tooltip-minimum-width 40)))

(defun nasy/orderless-dispatch-flex-first (_pattern index _total)
  "orderless-flex for corfu."
  (and (eq index 0) 'orderless-flex))

(defun nasy/setup-corfu ()
  "Setup corfu."
  (setq-local orderless-matching-styles '(orderless-flex)
    orderless-style-dispatchers nil)
  (add-hook 'orderless-style-dispatchers #'nasy/orderless-dispatch-flex-first nil 'local))

;; 暂时使用 company-mode
(when (string= my/completion-styles "corfu")
  (use-package corfu
    ;; Optional customizations
    ;; :custom
    ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
    ;; (corfu-auto t)                 ;; Enable auto completion
    ;; (corfu-separator ?\s)          ;; Orderless field separator
    ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
    ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
    ;; (corfu-preview-current nil)    ;; Disable current candidate preview
    ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
    ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
    ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
    ;; (corfu-scroll-margin 5)        ;; Use scroll margin

    ;; Enable Corfu only for certain modes.
    ;; :hook ((prog-mode . corfu-mode)
    ;;        (shell-mode . corfu-mode)
    ;;        (eshell-mode . corfu-mode))

    ;; Recommended: Enable Corfu globally.
    ;; This is recommended since Dabbrev can be used globally (M-/).
    ;; See also `corfu-excluded-modes'.
    :init
    (setq corfu-cycle t)
    (setq corfu-auto t)

    (setq corfu-quit-at-boundary t)
    (setq corfu-quit-no-match t)
    (setq corfu-preview-current nil)
    (setq corfu-min-width 80)
    (setq corfu-max-width 100)
    (setq corfu-auto-delay 0.2)
    (setq corfu-auto-prefix 1)
    (global-corfu-mode)
    :hook (prog-mode . nasy/setup-corfu)
    :config
    ;; 以下是官方代码
    ;; (defun corfu-enable-in-minibuffer ()
    ;;   "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    ;;   (when (where-is-internal #'completion-at-point (list (current-local-map)))
    ;;     ;; (setq-local corfu-auto nil) Enable/disable auto completion
    ;;     (corfu-mode 1)))
    ;; (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

    ;; 以下是官方代码
    (defun corfu-move-to-minibuffer ()
      (interactive)
      (let ((completion-extra-properties corfu--extra)
             completion-cycle-threshold completion-cycling)
        (toggle-chinese-search)
        (apply #'consult-completion-in-region completion-in-region--data)))
    (define-key corfu-map "\M-m" #'corfu-move-to-minibuffer)

    (define-key corfu-map (kbd "C-j") 'corfu-next)
    (define-key corfu-map (kbd "C-k") 'corfu-previous))

  ;; Use Dabbrev with Corfu!
  (use-package dabbrev
    ;; Swap M-/ and C-M-/
    :bind (("M-/" . dabbrev-completion)
            ("C-M-/" . dabbrev-expand))
    ;; Other useful Dabbrev configurations.
    :custom
    (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

  ;; A few more useful configurations...
  (use-package emacs
    :init
    ;; TAB cycle if there are only few candidates
    (setq completion-cycle-threshold 3)

    ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
    ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
    ;; (setq read-extended-command-predicate
    ;;       #'command-completion-default-include-p)

    ;; Enable indentation+completion using the TAB key.
    ;; `completion-at-point' is often bound to M-TAB.
    (setq tab-always-indent 'complete))

  ;; Add extensions
  (use-package cape
    ;; Bind dedicated completion commands
    :bind (("C-c p p" . completion-at-point) ;; capf
            ("C-c p t" . complete-tag)        ;; etags
            ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
            ("C-c p f" . cape-file)
            ("C-c p k" . cape-keyword)
            ("C-c p s" . cape-symbol)
            ("C-c p a" . cape-abbrev)
            ("C-c p i" . cape-ispell)
            ("C-c p l" . cape-line)
            ("C-c p w" . cape-dict)
            ("C-c p \\" . cape-tex)
            ("C-c p _" . cape-tex)
            ("C-c p ^" . cape-tex)
            ("C-c p &" . cape-sgml)
            ("C-c p r" . cape-rfc1345))
    :init
    (setq cape-dabbrev-min-length 3)
    ;; Add `completion-at-point-functions', used by `completion-at-point'.
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-tex)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (setq cape-dabbrev-check-other-buffers nil)
    (add-to-list 'completion-at-point-functions #'cape-keyword)
    (add-to-list 'completion-at-point-functions #'cape-history)
    (add-to-list 'completion-at-point-functions #'cape-dict)
    (add-to-list 'completion-at-point-functions #'cape-symbol)
    (add-to-list 'completion-at-point-functions #'cape-ispell))

  (require 'kind-all-the-icons)
  (add-to-list 'corfu-margin-formatters #'kind-all-the-icons-margin-formatter)
  )

;; minibuffer的增强
;; 增强 minibuffer 补全：vertico 和 Orderless
(use-package vertico ;;所有的minibuffer都适用
  :ensure t
  :config
  (vertico-mode t)
  )

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
    completion-category-defaults nil
    completion-category-overrides '((file (styles . (partial-completion))))))

;; 配置 Marginalia 增强 minubuffer 的 annotation
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode t)
  )

;;minibuffer action 和自适应的 context menu：Embark
(use-package embark
  :ensure t
  :config
  ;; 可以实现不用记快捷键，在minibuffer就能执行命令
  (setq prefix-help-command 'embark-prefix-help-command)
  :bind ("C-;" . embark-act)
  )

;;增强文件内搜索和跳转函数定义：Consult
(use-package consult
  :ensure t
  :bind
  ;;consult-line搜索
  ("C-s" . consult-line)
  ;;找到代码中自定义和函数或者使用的packages（前提是用use-package）
  ("C-c i" . consult-imenu)
  ("C-x C-r" . consult-recent-file))

(define-key minibuffer-local-map (kbd "C-c C-e") 'embark-export-write)

;;将自定义的函数加到embark-act中
(with-eval-after-load 'embark
  (define-key embark-file-map (kbd "E") #'consult-directory-externally))

;; ---------------------------------------------------------------------------

;; ---------------------------------------------------------------------------
;;增强 embark 和 consult，批量搜索替换大杀器
(use-package embark-consult
  :ensure t)

(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t))

(eval-after-load 'consult
  '(eval-after-load 'embark
     '(progn
        (require 'embark-consult)
        (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))))

;; ---------------------------------------------------------------------------

;;everyting
;;consult-locate
;; 配置搜索中文
(progn
  (setq consult-locate-args (encode-coding-string "es.exe -i -p -r" 'gbk))
  (add-to-list 'process-coding-system-alist '("es" gbk . gbk))
  )
(eval-after-load 'consult
  (progn
    (setq
      consult-narrow-key "<"
      consult-line-numbers-widen t
      consult-async-min-input 2
      consult-async-refresh-delay  0.15
      consult-async-input-throttle 0.2
      consult-async-input-debounce 0.1)
    )
  )

(provide 'init-completion)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-completion.el ends here
