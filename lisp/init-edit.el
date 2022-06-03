;;; init-edit.el --- Day-to-day editing helpers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))
(add-hook 'after-init-hook 'electric-indent-mode)


;;; Some basic preferences
(setq-default
 blink-cursor-interval 0.4
 bookmark-default-file (locate-user-emacs-file ".bookmarks.el")
 buffers-menu-max-size 30
 case-fold-search t
 column-number-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
 create-lockfiles nil
 auto-save-default nil
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 scroll-error-top-bottom t
 )

(add-hook 'after-init-hook 'delete-selection-mode)
(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
(with-eval-after-load 'autorevert
  (diminish 'auto-revert-mode))
(add-hook 'after-init-hook 'transient-mark-mode)

(fset 'yes-or-no-p 'y-or-n-p)


;;; Whitespace
(setq-default show-trailing-whitespace nil)

(defun show-trailing-whitespace ()
  "Enable display of trailing whitespace in this buffer."
  (setq-local show-trailing-whitespace t))

(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook 'show-trailing-whitespace))



;;; Display line numbers
(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))


;;; Display fill column indicator
(when (boundp 'display-fill-column-indicator)
;;  (setq-default indicate-buffer-boundaries 'left)
  (setq-default display-fill-column-indicator-column 80)
  (setq-default display-fill-column-indicator-character ?\u254e)
  (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode))


;;; Hideshow
 (add-hook 'prog-mode-hook 'hs-minor-mode)
 (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
 (global-set-key (kbd "C-M-[") 'hs-show-block)
 (global-set-key (kbd "C-M-]") 'hs-hide-block)


;;; Newline behaviour
;;; TTY not support
(global-set-key (kbd "RET") 'newline-and-indent)
(defun newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))
(defun newline-at-previous-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (newline-and-indent))

(global-set-key (kbd "C-<return>") 'newline-at-end-of-line)
(global-set-key (kbd "C-S-<return>") 'newline-at-previous-of-line)


;; Huge files
(when (fboundp 'so-long-enable)
  (add-hook 'after-init-hook 'so-long-enable))
(use-package vlf)
(defun ffap-vlf ()
  "Find file at point with VLF."
  (interactive)
  (let ((file (ffap-file-at-point)))
    (unless (file-exists-p file)
      (error "File does not exist: %s" file))
    (vlf file)))


;;; Move dup
(use-package move-dup
  :bind (("M-p"   . move-dup-move-lines-up)
         ("C-M-p" . move-dup-duplicate-up)
         ("M-n"   . move-dup-move-lines-down)
         ("C-M-n" . move-dup-duplicate-down)
         ))


;;; TTY copy&paste
(use-package clipetty
  :hook (after-init . global-clipetty-mode))


;;; Mode line bell
(use-package mode-line-bell
  :init
  (add-hook 'after-init-hook 'mode-line-bell-mode))


;;; Beacon
(use-package beacon
  :config
  (setq-default beacon-lighter "")
  (setq-default beacon-size 20)
  :init
  (add-hook 'after-init-hook 'beacon-mode))


;;; Rainbow delimiters
(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))


;;; Page break lines
(use-package page-break-lines
  :diminish page-break-lines-mode
  :init
  (add-hook 'after-init-hook 'global-page-break-lines-mode))


;;; Which key
(use-package which-key
  :diminish which-key-mode
  :config
  (setq-default which-key-idle-delay 1.5)
  :init
  (add-hook 'after-init-hook 'which-key-mode))


;;; Multiple cursors
;; (require-package 'multiple-cursors)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-+") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)



(provide 'init-edit)
;;; init-edit.el ends here
