;; -*- lexical-binding: t -*-

;; ---------------------------------------------------------------------------
;;company
;;放弃使用auto-complete，转用company
;;“C-n”和“C-p”来在补全提示栏中选择补全项
(use-package company
  :config
  (global-company-mode)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0))

;; ---------------------------------------------------------------------------

;;tab键补全
(setq tab-always-indent 'complete)

;; ---------------------------------------------------------------------------
;;vertico、orderless、marginalia、embark、consult和embark-consult的组合
;;可以很好的替代ivy和helm
;;minibuffer的增强
;;增强 minibuffer 补全：vertico 和 Orderless
(use-package vertico ;;所有的minibuffer都适用
  :ensure t
  :config
  (vertico-mode t)
  )

(use-package orderless
  :ensure t
  :config (setq completion-styles '(orderless)))

;;配置 Marginalia 增强 minubuffer 的 annotation
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode t)
  )

;;minibuffer action 和自适应的 context menu：Embark
(use-package embark
  :ensure t
  :init
  ;;可以实现不用记快捷键，在minibuffer就能执行命令
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
  ("C-x C-r" . consult-recent-file)
  )

;; ---------------------------------------------------------------------------

;; ---------------------------------------------------------------------------
;;增强 embark 和 consult，批量搜索替换大杀器
;;使用步骤：
;;1. 先使用consult-ripgrep查找一个字符串（英文），比如说是“hello”；
;;2. 输完之后不做其他操作，然后按快捷键“C-c C-e”；
;;3. 按完之后会列出你这个目录下所有包含“hello”的搜索信息；
;;4. 接着使用命令“M-x query-replace-regexp”（批量替换功能），对文本进行替换；
;;5. 最后按快捷键“C-c C-c”完成替换。
;;⚠️：
;;1. 可以替换ivy
;;2. 这套流程也适合在当前buffer里的批量替换，只不过一开始需要用快捷键“C-s”来查找
(use-package embark-consult :ensure t)

(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t))

;;add-hook的用法：
;;在一个mode加载完后加载另一个mode，也就是挂钩
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
