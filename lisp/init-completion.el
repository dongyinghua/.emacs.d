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
  :bind
  ("C-;" . embark-act)
  ("C-；" . embark-act)
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
  (define-key embark-file-map (kbd "C-f") #'consult-file-externally)
  (define-key embark-file-map (kbd "C-d") #'consult-directory-externally)
  (define-key embark-symbol-map (kbd "C-e") #'dragonli-eaf-search-bing-other-window)
  (define-key embark-region-map (kbd "C-e") #'dragonli-eaf-search-bing-other-window))

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

;; everyting + consult-locate
;; 需要在everthing的官网下载es.exe，然后将es.exe放到emacs安装目录下的bin目录中
;; 配置搜索中文
(progn
  (setq consult-locate-args (encode-coding-string "es.exe -i -p -r" 'gbk))
  (add-to-list 'process-coding-system-alist '("es" gbk . gbk)))

(provide 'init-completion)
;;; init-completion.el ends here
