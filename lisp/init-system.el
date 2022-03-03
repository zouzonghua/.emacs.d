;;; init-system.el --- configs for startup

;; Author: zouzonghua <zouzonghua.cn@gmail.com>
;; Version: 1.0
;; Homepage: https://github.com/zouzonghua

;;; Commentary:
;; (c) zouzonghua, 2022-

;;; code:

;;; flymake cannot find load-path solution
;; [refs] https://emacs-china.org/t/flymake/8323/19
(setq elisp-flymake-byte-compile-load-path
      (append elisp-flymake-byte-compile-load-path load-path))

;;; system coding
;; although others may add many other settings here,
;; but I think the next line is enough
(prefer-coding-system 'utf-8)

;;; emacs settings
(setq inhibit-startup-screen t  ; disable the startup screen splash
      ;; initial-major-mode 'fundamental-mode
      ;; initial-scratch-message nil
      make-backup-files nil             ; disable backup file
      scroll-preserve-screen-position 'always ; Preserve the cursor position relative to the screen when scrolling
      scroll-error-top-bottom t ; Move to beg/end of buffer before signalling an error
      )


;; settings for line number
(setq display-line-numbers-type 't) ; relative, visual
(global-display-line-numbers-mode t)

(provide 'init-system)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-system.el ends here
