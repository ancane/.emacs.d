(require 'popup-imenu)

(setq popup-imenu-use-flx t)
(setq imenu-auto-rescan t)

(global-set-key (kbd "C-e") 'popup-imenu)
