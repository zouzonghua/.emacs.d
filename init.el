;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)


(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-time) ;; Measure startup time

(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-gui* (window-system))
(defconst *is-a-tty* (not window-system))

;; Adjust garbage collection thresholds during startup, and thereafter
;; -*- lexical-binding: t -*-
(setq lexical-binding t)
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Bootstrap config
(eval-and-compile ; `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require 'borg)
  (borg-initialize))

(eval-and-compile ; `use-package'
  (require  'use-package)
  (setq use-package-verbose t))

(use-package dash)
(use-package eieio)

(use-package auto-compile
  :config
  (setq auto-compile-display-buffer               nil)
  (setq auto-compile-mode-line-counter            t)
  (setq auto-compile-source-recreate-deletes-dest t)
  (setq auto-compile-toggle-deletes-nonlib-dest   t)
  (setq auto-compile-update-autoloads             t))

(use-package epkg
  :defer t
  :init
  (setq epkg-repository
        (expand-file-name "var/epkgs/" user-emacs-directory))
  (setq epkg-database-connector
        (if (>= emacs-major-version 29) 'sqlite-builtin 'sqlite-module)))


(setq custom-file (locate-user-emacs-file "custom.el"))
;(require 'init-elpa)      ;; Machinery for installing required packages
;(require 'init-exec-path) ;; Set up $PATH

;; Load configs for specific features and modes
;(use-package diminish)
;(use-package command-log-mode)

(require 'init-kbd)
(require 'init-ui)
(require 'init-edit)
;(require 'init-buffer)
;
;(require 'init-company)
;(require 'init-flymake)
;(require 'init-eglot)
;
;(require 'init-javascript)
;(require 'init-markdown)

(provide 'init)

;; Allow access from emacsclient
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
