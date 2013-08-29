(require 'nav)

(global-set-key (kbd "<f12>") 'nav-toggle)

(add-hook 'psw-after-switch-hook 'nav-jump-to-current-dir)
