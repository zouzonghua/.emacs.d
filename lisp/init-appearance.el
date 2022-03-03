;;; init-appearance.el --- appearance config

;; Author: zouzonghua <zouzonghua.cn@gmail.com>
;; Version: 1.0
;; Homepage: https://github.com/zouzonghua

;;; Commentary:
;; (c) zouzonghua, 2022-

;;; code:

;; set default font
(set-face-attribute 'default nil
                    :family "MesloLGM Nerd Font"
                    :height 140
                    :weight 'normal
                    :width 'normal)
(toggle-frame-maximized)

(tool-bar-mode -1)      ; 隐藏工具栏
(scroll-bar-mode -1)    ; 隐藏滚动条
(global-hl-line-mode 1) ; 高亮当前行
;; (global-whitespace-mode 1) ; 显示 空格、换行、Tab

(provide 'init-appearance)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-appearance.el ends here
