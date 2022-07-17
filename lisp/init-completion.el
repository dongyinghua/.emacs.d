;;; init-company.el --- Initialize company configurations. -*- lexical-binding: t -*-

;;; Commentary
;;
;; Auto-completion configurations.
;;

;;; Code:

(require 'init-funcs)

;;company
;;放弃使用auto-complete，转用company
;;“C-n”和“C-p”来在补全提示栏中选择补全项
(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0))

;; org-bars 的文档中说到，如果同时使用 org-bars 和 company-mode，就需要使用company-posframe
(use-package company-posframe
  :hook (company-mode . company-posframe-mode)
  :config
  (setq company-tooltip-minimum-width 40))

;;tab键补全
(setq tab-always-indent 'complete)

;; minibuffer的增强
;; 增强 minibuffer 补全：vertico 和 Orderless
(use-package vertico ;;所有的minibuffer都适用
  :ensure t
  :config
  (vertico-mode t)
  )

(use-package orderless
  :ensure t
  :config (setq completion-styles '(orderless)))

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
