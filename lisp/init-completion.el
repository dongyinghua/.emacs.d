;; init-company.el --- Initialize company configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Auto-completion configurations.
;;

;;; Code:

(require 'init-funcs)
(require 'init-custom)

(use-package all-the-icons
  :ensure t
  :defer t)

;;company
;;放弃使用auto-complete，转用company
;;“C-n”和“C-p”来在补全提示栏中选择补全项
(use-package company
  :defer t
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 0)
  (setq company-idle-delay 0.0)
  (setq company-show-quick-access t) ; 给选项编号 (按快捷键 M-1、M-2 等等来进行选择).
  (setq company-selection-wrap-around t)
  (setq company-transformers '(company-sort-by-occurrence)) ; 根据选择的频率进行排序

  ;; company-tabnine AI 自动补全
  (use-package company-tabnine
    :ensure t
    :init
    (add-to-list 'company-backends #'company-tabnine)
    ;;(setq-default company-tabnine--disable-next-transform nil)

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
    (advice-add #'company-tabnine :around #'my-company-tabnine)
    )
  )

;; 图形界面
(use-package company-box
  :ensure t
  :defer t
  :hook (company-mode . company-box-mode))

;; org-bars 的文档中说到，如果同时使用 org-bars 和 company-mode，就需要使用company-posframe
(use-package company-posframe
  :ensure t
  :defer t
  :hook (company-mode . company-posframe-mode)
  :config (setq company-tooltip-minimum-width 40))

;; tab键补全
;; 如果 company-show-numbers 为 non-nil以及使用 company-tabnine，就无法使用 <tab> 键补全了
(add-hook 'after-init-hook #'(lambda () (setq tab-always-indent 'complete)))

;; minibuffer的增强
;; 增强 minibuffer 补全：vertico 和 Orderless
(use-package vertico ;;所有的minibuffer都适用
  :ensure t
  :defer t
  :hook (after-init . vertico-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :ensure t
  :defer t
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
  :defer t
  :hook (after-init . marginalia-mode))

;;minibuffer action 和自适应的 context menu：Embark
(use-package embark
  :ensure t
  :defer t
  :bind ("C-;" . embark-act)
  :config
  ;; 可以实现不用记快捷键，在minibuffer就能执行命令
  (setq prefix-help-command 'embark-prefix-help-command)
  )

;;增强文件内搜索和跳转函数定义：Consult
(use-package consult
  :ensure t
  :defer t
  :bind
  ;;consult-line搜索
  ("C-s" . consult-line)
  ;;找到代码中自定义和函数或者使用的packages（前提是用use-package）
  ("C-c i" . consult-imenu)
  ("C-x C-r" . consult-recent-file)
  ("C-c C-p r" . consult-ripgrep)
  :config
  (setq
    consult-narrow-key "<"
    consult-line-numbers-widen t
    consult-async-min-input 2
    consult-async-refresh-delay  0.15
    consult-async-input-throttle 0.2
    consult-async-input-debounce 0.1))

(define-key minibuffer-local-map (kbd "C-c C-e") 'embark-export-write)

;;将自定义的函数加到 embark-file-map 中
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
        (add-hook 'embark-collect-mode-hook 'consult-preview-at-point-mode))))

;; ---------------------------------------------------------------------------

;;everyting
;;consult-locate
;; 配置搜索中文
;; (progn
;;   (setq consult-locate-args (encode-coding-string "es.exe -i -p -r" 'gbk))
;;   (add-to-list 'process-coding-system-alist '("es" gbk . gbk)))

(provide 'init-completion)
;;; init-completion.el ends here
