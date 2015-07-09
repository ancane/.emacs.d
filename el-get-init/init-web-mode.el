(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.html?\\'"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.xhtml?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ssp?\\'" . web-mode))


(defun web-mode-hook ()
  (progn
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset    4)
    (setq web-mode-code-indent-offset   4)

    (setq web-mode-style-padding        1)
    (setq web-mode-script-padding       1)
    (setq web-mode-block-padding        0)
    (local-unset-key (kbd "C-;"))
    )
  )

(add-hook 'web-mode-hook  'web-mode-hook)
