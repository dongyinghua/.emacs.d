;;; init-company.el --- Company-mode configuration -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Company-mode configuration

;;; Code:
;;company
;;放弃使用auto-complete，转用company
;;“C-n”和“C-p”来在补全提示栏中选择补全项
(use-package company
  :defer t
  :ensure t
  :hook(text-mode . company-mode)
  :config
  (global-company-mode)
  (setq company-global-modes '(not prog-mode))
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 1.0)
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
  );; use-package company


(provide 'init-company)
;;; init-company.el ends here
