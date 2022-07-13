;; init-hydra.el --- Initialize hydra configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Nice looking hydras.
;; 借鉴自seagle0128的init-hydra.el
;;

;;; Code:

(require 'init-custom)
(require 'init-funcs)

;; 关于hydra的配置应当写在靠前一点的位置比较保险。
(use-package hydra
  :ensure t)

(use-package pretty-hydra
  :bind ("C-c <f1>" . toggles-hydra/body)
  :init
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                 &key face height v-adjust)
    "Add an icon in the hydra title."
    (let ((face (or face `(:foreground ,(face-background 'highlight))))
           (height (or height 1.0))
           (v-adjust (or v-adjust 0.0)))
      (concat
        (when (and (icon-displayable-p) icon-type icon-name)
          (let ((f (intern (format "all-the-icons-%s" icon-type))))
            (when (fboundp f)
              (concat
                (apply f (list icon-name :face face :height height :v-adjust v-adjust))
                " "))))
        (propertize title 'face face))))

  ;; Global toggles
  (with-no-warnings
    (pretty-hydra-define toggles-hydra
      (:title (pretty-hydra-title "Emacs toggles hydras" 'fileicon "emacs" :face 'all-the-icons-pink :height 1.0 :v-adjust -0.15)
        :color amaranth :quit-key "q")
      ("Basic"
        (("n" (if (fboundp 'display-line-numbers-mode)
                (display-line-numbers-mode (if display-line-numbers-mode -1 1))
                (global-display-line-numbers-mode (if global-display-line-numbers-mode -1 1)))
           "line number"
           :toggle (or (bound-and-true-p display-line-numbers-mode) global-display-line-numbers-mode))
          ;;("a" global-aggressive-indent-mode "aggressive indent" :toggle t)
          ("d" global-hungry-delete-mode "hungry delete" :toggle t)
          ("e" electric-pair-mode "electric pair" :toggle t)
          ("c" flyspell-mode "spell check" :toggle t)
          ("s" prettify-symbols-mode "pretty symbol" :toggle t)
          ("l" global-page-break-lines-mode "page break lines" :toggle t)
          ("b" display-battery-mode "battery" :toggle t)
          ("i" display-time-mode "time" :toggle t)
          ("m" doom-modeline-mode "modern mode-line" :toggle t)
          ("N" nyan-mode "nyan mode" :toggle t)
          ("r" restart-emacs "restart emacs" :toggle nil :exit t))
        "Theme"
        ()
        "Package Archive"
        (("p m" (centaur-set-package-archives 'melpa t)
           "melpa" :toggle (eq centaur-package-archives 'melpa) :exit t)
          ("p c" (centaur-set-package-archives 'emacs-cn t)
            "emacs-cn" :toggle (eq centaur-package-archives 'emacs-cn) :exit t)
          ("p b" (centaur-set-package-archives 'bfsu t)
            "bfsu" :toggle (eq centaur-package-archives 'bfsu) :exit t)
          ("p n" (centaur-set-package-archives 'netease t)
            "netease" :toggle (eq centaur-package-archives 'netease) :exit t)
          ("p s" (centaur-set-package-archives 'sjtu t)
            "sjtu" :toggle (eq centaur-package-archives 'sjtu) :exit t)
          ("p t" (centaur-set-package-archives 'tuna t)
            "tuna" :toggle (eq centaur-package-archives 'tuna) :exit t)
          ("p u" (centaur-set-package-archives 'ustc t)
            "ustc" :toggle (eq centaur-package-archives 'ustc) :exit t)
          ("p T" (centaur-test-package-archives t) "speed test" :exit t)))
      )
    )
  ;;:config
  ;;(evil-global-set-key 'normal (kbd "SPC") 'toggles-hydra/body)
  )

(provide 'init-hydra)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-hydra.el ends here
