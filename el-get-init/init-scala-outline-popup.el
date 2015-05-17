(require 'scala-outline-popup)

(setq scala-outline-popup-select 'closest)

(add-hook
 'scala-mode-hook
 (lambda () (local-set-key (kbd "C-e") 'scala-outline-popup)))
