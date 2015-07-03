(require 'scala-outline-popup)

(setq scala-outline-popup-select 'closest)
(setq scala-outline-popup-use-flx t)
(setq scala-outline-popup-position 'point)

(add-hook
 'scala-mode-hook
 (lambda () (local-set-key (kbd "C-\\") 'scala-outline-popup)))
