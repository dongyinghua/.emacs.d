;; -*- lexical-binding: t -*-
;;子龙山人的镜像速度快，但是package的版本可能落后于官方源
(use-package package
  :config
  (setq package-archives '(("gnu"   . "http://elpa.zilongshanren.com/gnu/")
                            ("melpa" . "http://elpa.zilongshanren.com/melpa/")))
  (package-initialize)

  ;;防止反复调用 package-refresh-contents 会影响加载速度
  (when (not package-archive-contents)
    (package-refresh-contents)
    )
  )

;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize)
  )

;; ---------------------------------------------------------------------------
;;company
;;放弃使用auto-complete，转用company
;;“C-n”和“C-p”来在补全提示栏中选择补全项
(use-package company
  :config
  (global-company-mode)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0)
  )

;; ---------------------------------------------------------------------------


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
;;从Emacs打开电脑文件
;;1. 用快捷键"C-x C-f"先选定一个文件或文件夹，然后按快捷键“C-;”；
;;2. 就会看到*Embark Actions*中有consult-directory-externally这一项；
;;3. 接着按“E”就会打开这个文件或文件夹。
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

;;将自定义的函数加到embark-act中
;;注意“with-eval-after-load”和“eval-after-load”在使用上的区别：
;;使用“with-eval-after-load”时，(define-key embark-file-map (kbd "E") #'consult-directory-externally)不用加单引号，而使用“eval-after-load”时则需要加单引号
;;以下两种方式的效果应该是一样的
;;(with-eval-after-load 'embark
;;    (define-key embark-file-map (kbd "E") #'consult-directory-externally))
;;以下面的语句为例，解释一下“eval-after-load”的作用
;;“eval-after-load”表示在加载embark之后再执行后边的语句
(eval-after-load 'embark
  '(define-key embark-file-map (kbd "E") #'consult-directory-externally))

;; ---------------------------------------------------------------------------

;; ---------------------------------------------------------------------------
;;使用ripgrep来进行搜索（是在“~/.emacs”目录下搜索的）
;;ripgrep使用Rust语言编写的
;;consult-ripgrep
;;直接执行“M-x consult-ripgrep”会报错，说找不到“rg”。
;;解决办法：
;;1. 在网站“https://github.com/BurntSushi/ripgrep/releases”中下载对应macOS的压缩包（-apple-darwin.tar.gz）；
;;2. 下载完后解压，然后在压缩包中会找到一个可执行文件rg；
;;3. 将压缩包中的可执行文件rg复制到目录“/opt/homebrew/Cellar/emacs-mac/emacs-28.1-mac-9.0/libexec/emacs/28.1/aarch64-apple-darwin21.4.0”中。
;;⚠️
;;1. 执行rg的时候macOS会提示这个可执行文件未收到信任，这个时候直接全部信任就行了，应该不会出现什么问题。
;;2. rg文件的复制目录可能不完全一样，只要总体上的位置对就行
;;3. consult-ripgrep不支持中文查找
;;4. 有Grep和Find功能的consult命令（consult-ripgrep、consult-find、consult-locate等），可以利用在minibuffer中使用“#”筛选检索或查找的结果
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
(define-key minibuffer-local-map (kbd "C-c C-e") 'embark-export-write)

;; ---------------------------------------------------------------------------

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

;; ---------------------------------------------------------------------------


;; ---------------------------------------------------------------------------
;;visual-fill-column-mode
;;https://codeberg.org/joostkremers/visual-fill-column
(use-package visual-fill-column
  :ensure t
  :init
  ;;在所有由text-mode衍生出来的mode中禁用toggle-truncate-lines
  (defun toggle-truncate-lines-off()
    (interactive)
    (setq truncate-lines nil)
    )
  ;;此处加“#”的目的是为了标记后边是函数，也可以不加
  (add-hook 'text-mode-hook 'toggle-truncate-lines-off)

  :config
  (global-visual-fill-column-mode)
  (setq-default visual-fill-column-center-text t)
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)
  ;;(setq-local fill-column 180)
  ;;visual-fill-column-extra-text-width可以调节文本在中间时，文本两边距屏幕边缘的距离
  (setq-default visual-fill-column-extra-text-width '(10 . 10))

  ;;在所有从text-mode衍生出来的mode中使用visual-fill-column-mode
  (add-hook 'text-mode-hook 'visual-fill-column-mode)
  )

;; ---------------------------------------------------------------------------

;;代码格式.editorconfig
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;;unicode-fonts
(use-package unicode-fonts
  :config
  (unicode-fonts-setup))

;;evil模式
(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  ;;下面的代码可以将 insert state map 中的快捷键清空，使其可以回退（Fallback）到 Emacs State 中，
  ;;这样我们之前的 Emacs State 里面定义的 C-w 等快捷键就不会被 evil insert minor mode state 所覆盖。
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map [escape] 'evil-normal-state)
  )

;; markdown
(use-package markdown-mode
  :ensure t)
(provide 'init-packages)
