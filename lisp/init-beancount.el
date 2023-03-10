;;; init-beancount.el --- Beancount support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package beancount
  :mode ("\\.bean\\'" . beancount-mode)
  :init
  (add-hook 'beancount-mode-hook #'outline-minor-mode))

(provide 'init-beancount)
;;; init-beancount.el ends here
