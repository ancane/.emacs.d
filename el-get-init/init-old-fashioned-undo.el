(require 'old-fashioned-undo)

(old-fashioned-undo-mode t)

(global-set-key (kbd "C-z")   'undo)
(global-set-key (kbd "C-S-z") 'redo)
