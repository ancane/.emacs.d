(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark-ancane)))
 '(custom-safe-themes (quote ("9bd0bf1768ccdad78e3269a9200ac031ac42db2a711c06e43b4c3d13dea33438" default)))
 '(custom-theme-directory "~/.emacs.d/custom/themes")
 '(nav-boring-file-regexps (quote ("^[.]$" "~$" "[.]elc$" "[.]pyc$" "[.]o$" "[.]bak$" "^_MTN$" "^blib$" "^CVS$" "^RCS$" "^SCCS$" "^_darcs$" "^_sgbak$" "^autom4te.cache$" "^cover_db$" "^_build$" "^[.]git$" "^[.]ensime")))
 '(safe-local-variable-values (quote ((eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook" (add-hook (quote write-contents-functions) (lambda nil (delete-trailing-whitespace) nil)) (require (quote whitespace)) "Sometimes the mode needs to be toggled off and on." (whitespace-mode 0) (whitespace-mode 1)) (whitespace-line-column . 80) (whitespace-style face trailing lines-tail) (require-final-newline . t))))
 '(projectile-keymap-prefix (kbd "C-c C-p"))
)

;; Custom init files
(add-to-list 'load-path "~/.emacs.d/custom")

;; Load custom init files
(load "init-general.el")
(load "init-el-get.el")
(load "init-func.el")
(load "init-ibuffer.el")
(load "init-keys.el")
