;;; init-buffer.el --- Config for minibuffer completion       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


(use-package vertico)
(vertico-mode t)


(use-package orderless)
(setq completion-styles '(orderless))


(use-package marginalia)
(marginalia-mode t)


(use-package embark)
(global-set-key (kbd "C-;") 'embark-act)
(setq prefix-help-command 'embark-prefix-help-command)


(use-package consult)
(global-set-key (kbd "C-s") 'consult-line)

(provide 'init-buffer)
;;; init-buffer.el ends here
