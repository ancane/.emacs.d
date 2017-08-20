(require 'popup-imenu)

(setq popup-imenu-use-flx t)
(setq popup-imenu-position 'point)
(setq imenu-auto-rescan t)

(setq popup-imenu-style 'indent)

(global-set-key (kbd "C-e") 'popup-imenu)


(setq popup-imenu-position 'point)
(setq popup-imenu-style 'indent)
(global-set-key (kbd "C-p") 'popup-imenu)
(define-key popup-isearch-keymap (kbd "C-p") 'popup-isearch-cancel)
