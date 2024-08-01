;;; init-org.el --- Initialize org configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Org configurations.
;;

;;; Code:

(require 'init-const)
(require 'init-custom)

(use-package org
  :ensure nil
  :defer 1
  :bind
  ("C-c b" . org-switchb)
  ("C-c 。" . org-time-stamp)
  ("C-c r" . org-refile)
  ("C-c t" . org-tags-sparse-tree)
  ("C-c g" . org-mark-ring-goto)
  ("C-c m" . org-mark-ring-push)
  ("C-c a" . org-agenda)
  ("C-c x" . org-capture)
  ("C-c C-f a" . consul-org-agenda)
  :config
  (define-key org-mode-map (kbd "C-c C-，") 'org-insert-structure-template)

  ;; 解决：Warning (org-element-cache): org-element--cache: Org parser error in slides.org::2206. Resetting.
  ;; The error was: (error "Invalid search bound (wrong side of point)")
  (customize-set-variable 'warning-suppress-log-types '((org-element-cache)))

  ;; org for GTD
  (require 'init-org-gtd)
  
  ;; 使 org-mode 中的 timestamp 格式为英文
  (setq system-time-locale "C")
  
  ;; Org LaTeX
  ;; 使用 XeLaTeX 程序进行编译转换
  (setq-default org-latex-compiler "xelatex")
  (setq-default org-latex-pdf-process '("xelatex %f"))
  (add-to-list 'org-latex-default-packages-alist '("" "ctex" t ("xelatex")))

  (setq-default org-latex-prefer-user-labels t)
  
  ;; 解决org-mode中LaTeX数学公式中的中文渲染问题
  ;; https://q3yi.me/post/4_use_xelatex_instead_of_latex_in_org_preview_latex_process/
  (add-to-list 'org-preview-latex-process-alist
	       '(xdvsvgm :progams
			 ("xelatex" "dvisvgm")
			 :discription "xdv > svg"
			 :message "you need install the programs: xelatex and dvisvgm."
			 :image-input-type "xdv"
			 :image-output-type "svg"
			 :image-size-adjust (4.5 . 4.5) ; 调整 svg 的 size
			 :latex-compiler ("xelatex -interaction nonstopmode -no-pdf -output-directory %o %f")
			 :image-converter ("dvisvgm %f -n -b min -c %S -o %O")))

  (setq org-preview-latex-default-process 'xdvsvgm)

  ;; org-mode美化公式预览https://emacs-china.org/t/org-mode-latex-mode/22490
  ;; 只借鉴了公式编号部分
  ;; from: https://kitchingroup.cheme.cmu.edu/blog/2016/11/07/
  ;; Better-equation-numbering-in-LaTeX-fragments-in-org-mode/
  (defun org-renumber-environment (orig-func &rest args)
    (let ((results '()) 
          (counter -1)
          (numberp))
      (setq results (cl-loop for (begin .  env) in 
                             (org-element-map (org-element-parse-buffer)
                                 'latex-environment
                               (lambda (env)
                                 (cons
                                  (org-element-property :begin env)
                                  (org-element-property :value env))))
                             collect
                             (cond
                              ((and (string-match "\\\\begin{equation}" env)
                                    (not (string-match "\\\\tag{" env)))
                               (cl-incf counter)
                               (cons begin counter))
                              ((and (string-match "\\\\begin{align}" env)
                                    (string-match "\\\\notag" env))
                               (cl-incf counter)
                               (cons begin counter))
                              ((string-match "\\\\begin{align}" env)
                               (prog2
                                   (cl-incf counter)
                                   (cons begin counter)                          
                                 (with-temp-buffer
                                   (insert env)
                                   (goto-char (point-min))
                                   ;; \\ is used for a new line. Each one leads
                                   ;; to a number
                                   (cl-incf counter (count-matches "\\\\$"))
                                   ;; unless there are nonumbers.
                                   (goto-char (point-min))
                                   (cl-decf counter
                                            (count-matches "\\nonumber")))))
                              (t
                               (cons begin nil)))))
      (when (setq numberp (cdr (assoc (point) results)))
        (setf (car args)
              (concat
               (format "\\setcounter{equation}{%s}\n" numberp)
               (car args)))))
    (apply orig-func args))
  (advice-add 'org-create-formula-image :around #'org-renumber-environment)
  
  ;; To speed up startup, don't put to init section
  (setq org-modules nil)     ;; Faster loading
  (setq org-startup-numerated t)

  (setq org-image-actual-width nil)
  (setq-default org-startup-with-inline-images t)

  ;; 单独设置org标题字体大小，https://emacs-china.org/t/org/12869
  ;; 设置org标题1-8级的字体大小和颜色，颜色摘抄自monokai
  ;; 希望org-mode标题的字体大小和正文一致，设成1.0， 如果希望标题字体大一点可以设成1.2
  ;; org-mode正文height为140
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 190))))
   '(org-level-2 ((t (:inherit outline-2 :height 160))))
   '(org-level-3 ((t (:inherit outline-3 :height 150))))
   '(org-level-4 ((t (:inherit outline-4 :height 150))))
   ) ;; end custom-set-faces

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (C . t)
     (java . t)
     (js . t)
     (ruby . t)
     (ditaa . t)
     (python . t)
     (shell . t)
     (latex . t)
     (plantuml . t)
     (R . t)))
  
  ;; 标题下的列表就可以像标题一样折叠了
  (setq org-cycle-include-plain-lists 'integrate)
  ) ; use-package org ends here

(use-package hi-lock
  :ensure nil
  :init
  (global-hi-lock-mode 1)
  (setq hi-lock-file-patterns-policy #'(lambda (dummy) t))
  :config
  (add-hook 'org-mode-hook 'org-bold-highlight)
  )

(use-package org-superstar
  :ensure t
  :if (and (display-graphic-p) (char-displayable-p ?◉))
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("▼")) ; no bullets
  (setq org-ellipsis " ▾")
  )

;; https://github.com/casouri/valign
;; 表格对齐
(use-package valign
  :ensure t
  :defer t
  :hook (org-mode . valign-mode)
  :config (setq valign-fancy-bar t))

;; (use-package org-contrib
;;   :pin nongnu
;;   :config
;;   ;; 对于需要重复完成的任务很有帮助
;;   (require 'org-checklist)
;;   )


;;（造轮子）定义了一个函数可以循环org-mode的emphasis-markers的可见性。
;; emphasis-markers就是org-mode轻语言的标记符号，比如说*、-等。
;; (defun org-cycling-emphasis-markers()
;;   (interactive)
;;   (if org-hide-emphasis-markers
;;     (setq org-hide-emphasis-markers nil)
;;     (setq org-hide-emphasis-markers t))
;;   (revert-buffer nil t nil))
;; (global-set-key (kbd "C-c c") 'org-cycling-emphasis-markers)

;; 但是有个更好的插件可以解决我这个需求：org-appear，以上就属于“造轮子”了
;; Github：https://github.com/awth13/org-appear
;; emacs-china：https://emacs-china.org/t/org-mode/16826
(use-package org-appear
  :ensure t
  :defer t
  :hook (org-mode . org-appear-mode)
  :config
  ;;使用evil-mode后，可以用以下代码来实现只在编辑模式下激活org-appear-mode
  ;;⚠️得先执行(setq org-hide-emphasis-markers t)，否则org-appear-autoemphasis会失效
  (setq org-hide-emphasis-markers t)
  (setq org-appear-autoemphasis t)
  (setq org-appear-autolinks t)
  (setq org-appear-trigger 'manual)
  (add-hook 'org-mode-hook (lambda ()
                             (add-hook 'evil-insert-state-entry-hook
				       'org-appear-manual-start
				       nil
				       t)
                             (add-hook 'evil-insert-state-exit-hook
				       'org-appear-manual-stop
				       nil
				       t)))) ; use-package org-appear

(use-package org-fragtog
  :ensure t
  :defer t
  :hook (org-mode . org-fragtog-mode)
  ;; (org-roam-mode . org-fragtog-mode)
  )

(use-package org-download
  :ensure t
  :defer t
  :hook
  (org-mode . org-download-enable)
  (org-roam-mode . org-download-enable)
  :config
  (setq-default org-download-heading-lvl 4)
  (setq-default org-download-image-dir "./images")
  ;; (defun dummy-org-download-annotate-function (link)
  ;; "")
  ;; (setq org-download-annotate-function
  ;; 'dummy-org-download-annotate-function)
  ;; (setq org-download-screenshot-method "imagemagick/convert -a -f %s")

  (defun org-screenshot-on-windows11 ()
    (interactive)
    (setq full-file-name (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
    ;; 如果文件名的长度小于14,放到mainImage文件夹下面
    (if (< (length full-file-name) 14)
	(setq before-file-name-part "main")
      ;;否则,判断文件中是否含有中文(专门给org roam做的优化,不通用,但是也不想改了)
      (if (string-match "\\cc" full-file-name)
          (setq before-file-name-part  (substring (file-name-sans-extension (file-name-nondirectory buffer-file-name)) 0 14))
	(setq before-file-name-part (substring (file-name-sans-extension (file-name-nondirectory buffer-file-name)) 15))))
    ;; 自己改动的地方，源代码：(setq imagefile (concat "./" before-file-name-part "Image/"))
    (setq imagefile (concat "./images/" (org-get-heading) "/"))
    (unless (file-exists-p imagefile)
      (make-directory imagefile))
    (setq filename (concat (make-temp-name (concat imagefile
                                                   (format-time-string "%Y%m%d_%H%M%S_")))
                           ".png"))
    (shell-command (concat "powershell -command \"Add-Type -AssemblyName System.Windows.Forms;if ($([System.Windows.Forms.Clipboard]::ContainsImage())) {$image = [System.Windows.Forms.Clipboard]::GetImage();[System.Drawing.Bitmap]$image.Save('"
                           filename "',[System.Drawing.Imaging.ImageFormat]::Png); Write-Output 'clipboard content saved as file'} else {Write-Output 'clipboard does not contain image data'}\""))
    (insert (concat "[[file:" filename "]]"))
    (org-display-inline-images))
  )

(provide 'init-org)
;;; init-org.el ends here
