;;; init-eaf.el --- eaf-mode configuration -*- lexical-binding: t -*-

;;; Commentary:
;;
;; eaf-mode configuration

;;; Code:
(use-package eaf
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
  :custom
  ;; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  (browse-url-browser-function 'eaf-open-browser)
  :config
  ;; (setq eaf-python-command "D:/develop/python3124/python.exe")
  ;; (setq eaf-enable-debug t)
  ;; (require 'eaf-demo)
  (require 'eaf-org)
  
  (require 'eaf-image-viewer)
  
  ;; eaf-browser
  (require 'eaf-browser)
  (setq eaf-browser-dark-mode nil)
  
  ;; eaf-pdf-viewer
  ;; 黑暗背景
  (require 'eaf-pdf-viewer)
  (setq eaf-pdf-dark-mode nil)

  ;; eaf-git
  ;; (require 'eaf-git)
  
  ;; (setq eaf-internal-process-prog "clang")
  ;; (defalias 'browse-web #'eaf-open-browser)
  ;; (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  ;; (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  ;; (eaf-bind-key take_photo "p" eaf-camera-keybinding)
  ;; (eaf-bind-key nil "M-q" eaf-browser-keybinding)
  ) ;; unbind, see more in the Wiki

(provide 'init-eaf)
;;; init-eaf.el ends here
