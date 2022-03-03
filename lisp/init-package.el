;;; init-package.el --- initialize the plugins

;; Author: zouzonghua <zouzonghua.cn@gmail.com>
;; Version: 1.0
;; Homepage: https://github.com/zouzonghua

;;; Commentary:
;; (c) zouzonghua, 2022-

;;; Code:

;; settings for company
(use-package company
  :diminish
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-show-quick-access t))

;; Add some color.
 (use-package diredfl
  :init (diredfl-global-mode 1))

;; theme
(package-install 'vs-dark-theme)
(load-theme 'vs-dark t) ; 设置主题

(provide 'init-package)


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-package.el ends here
