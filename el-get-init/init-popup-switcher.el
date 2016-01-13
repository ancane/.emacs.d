(require 'popup-switcher)

(global-set-key (kbd "C-;") 'psw-switch-buffer)
(global-set-key (kbd "C-'") 'psw-switch-projectile-files)

(setq psw-mark-modified-buffers t)
(setq psw-use-flx t)
