(require 'js2-mode)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(setq js2-auto-indent-p t)
(setq js2-cleanup-whitespace t)
(setq js2-enter-indents-newline t)
(setq js2-indent-on-enter-key t)
(setq js2-mirror-mode nil)
(setq js2-mode-indent-ignore-first-tab t)

(setq js2-global-externs
      '("module" "require" "console" "JSON" "$" "_"))

(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

;; Lets try tern mode
(add-to-list 'load-path "~/tern/emacs/")
(autoload 'tern-mode "tern.el" nil t)

(add-hook 'js-mode-hook (lambda () (tern-mode t)))
