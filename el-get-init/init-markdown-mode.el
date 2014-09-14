(require 'markdown-mode)

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)

(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(eval-after-load "markdown-mode"
  '(progn
    (define-key markdown-mode-map (kbd "M-<up>") nil)
    (define-key markdown-mode-map (kbd "M-<down>") nil)
    (define-key markdown-mode-map (kbd "M-<left>") nil)
    (define-key markdown-mode-map (kbd "M-<right>") nil)
    (define-key markdown-mode-map (kbd "M-<return>") nil)
    ))
