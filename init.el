;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)


(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-startuptime) ;; Measure startup time

(defconst *is-a-mac* (eq system-type 'darwin))

;; Adjust garbage collection thresholds during startup, and thereafter
;; -*- lexical-binding: t -*-
(setq lexical-binding t)
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Bootstrap config
(setq custom-file (locate-user-emacs-file "custom.el"))
(require 'init-elpa)      ;; Machinery for installing required packages

;; Load configs for specific features and modes
(use-package diminish)
(use-package command-log-mode)

(require 'init-kbd)
(require 'init-ui)
(require 'init-edit)
(require 'init-buffer)

(provide 'init)

;;; init.el ends here
