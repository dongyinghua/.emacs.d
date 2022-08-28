;;; init-eaf.el --- Initialize eaf configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package eaf
  :load-path "~/.emacs.d/tools/emacs-application-framework"
  :custom
  ;; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  (browse-url-browser-function 'eaf-open-browser)
  :init
  (setq eaf--mac-enable-rosetta t)
  :config
  (require 'eaf-org))

(provide 'init-eaf)
;;; init-eaf.el ends here
