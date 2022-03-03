;;; init-builtin --- initialize the builtin plugins

;; Author: zouzonghua <zouzonghua.cn@gmail.com>
;; Version: 1.0
;; Homepage: https://github.com/zouzonghua

;;; Commentary:
;; (c) zouzonghua, 2022-

;;; Code:

;; alias for yes-or-no-pair
(defalias 'yes-or-no-p 'y-or-n-p)

;; make tab-width always 2
(setq-default tab-width 2)
;; only use spaces instead of TAB, use C-q TAB to input the TAB char
(setq-default indent-tabs-mode nil)

;; my fix for tab indent
(defun zilongshanren/indent-region(numSpaces)
  (progn
                                      ; default to start and end of current line
    (setq regionStart (line-beginning-position))
    (setq regionEnd (line-end-position))

                                      ; if there's a selection, use that instead of the current line
    (when (use-region-p)
      (setq regionStart (region-beginning))
      (setq regionEnd (region-end))
      )

    (save-excursion                          ; restore the position afterwards
      (goto-char regionStart)                ; go to the start of region
      (setq start (line-beginning-position)) ; save the start of the line
      (goto-char regionEnd)                  ; go to the end of region
      (setq end (line-end-position))         ; save the end of the line

      (indent-rigidly start end numSpaces) ; indent between start and end
      (setq deactivate-mark nil)           ; restore the selected region
      )
    )
  )


(defun zilongshanren/tab-region ()
  (interactive)
  (if (use-region-p)
      (zilongshanren/indent-region 4)               ; region was selected, call indent-region
    (insert "    ")                   ; else insert four spaces as expected
    ))

(defun zilongshanren/untab-region ()
  (interactive)
  (zilongshanren/indent-region -4))

(defun zilongshanren/hack-tab-key ()
  (interactive)
  (local-set-key (kbd "<tab>") 'zilongshanren/tab-region)
  (local-set-key (kbd "<S-tab>") 'zilongshanren/untab-region)
  )

(add-hook 'prog-mode-hook 'zilongshanren/hack-tab-key)
(add-hook 'text-mode-hook 'zilongshanren/hack-tab-key)


;; Auto Complete
(fido-mode t)

;; Flymake
 (use-package flymake
   :ensure nil
   :hook (prog-mode . flymake-mode)
   :config
   (global-set-key (kbd "M-n") #'flymake-goto-next-error)
   (global-set-key (kbd "M-p") #'flymake-goto-prev-error))

 (provide 'init-builtin)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-builtin.el ends here
