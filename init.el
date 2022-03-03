;;; init.el --- the entry of emacs config

;; Author: zouzonghua <zouzonghua.cn@gmail.com>
;; Version: 1.0
;; Homepage: https://github.com/zouzonghua

;;; Commentary:
;; (c) zouzonghua, 2022-


;;; code:

;; adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; update the load-path
(setq default-directory "~/")
(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "lisp/")))

;; settings for independent packages and etc.
(require 'init-appearance)
;;(require 'init-fn)
(require 'init-system)
(require 'init-elpa)
(require 'init-package)
(require 'init-builtin)
;;(require 'init-lang)


;; load custom file at last
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)

;;; init.el ends here
