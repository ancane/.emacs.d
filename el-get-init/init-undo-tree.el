(require 'undo-tree)

(global-undo-tree-mode)

(defalias 'redo 'undo-tree-redo)

(global-set-key (kbd "C-z")   'undo)
(global-set-key (kbd "C-S-z") 'redo)
