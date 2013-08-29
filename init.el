(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark-ancane)))
 '(custom-safe-themes (quote ("9bd0bf1768ccdad78e3269a9200ac031ac42db2a711c06e43b4c3d13dea33438" default)))
 '(custom-theme-directory "~/.emacs.d/custom/themes"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Custom init files
(add-to-list 'load-path "~/.emacs.d/custom")

;; Load custom init files
(load "init-general.el")
(load "init-el-get.el")
(load "init-func.el")
(load "init-ibuffer.el")
(load "init-ido.el")
