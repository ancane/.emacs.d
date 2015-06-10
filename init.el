(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (quote browse-url-chromium))
 '(custom-enabled-themes (quote (tango-dark-ancane)))
 '(custom-safe-themes
   (quote
    ("450850644a55da3343a49d10344986a058ca17c449d3470dc302e56294ea1db8" default)))
 '(custom-theme-directory "~/.emacs.d/custom/themes")
 '(markdown-command "multimarkdown")
 '(nav-boring-file-regexps
   (quote
    ("^[.]$" "~$" "[.]elc$" "[.]pyc$" "[.]o$" "[.]bak$" "^_MTN$" "^blib$" "^CVS$" "^RCS$" "^SCCS$" "^_darcs$" "^_sgbak$" "^autom4te.cache$" "^cover_db$" "^_build$" "^[.]git$" "^[.]ensime")))
 '(projectile-keymap-prefix (kbd "C-c C-p"))
 '(restclient-print-curl t)
 '(safe-local-variable-values
   (quote
    ((eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook"
           (add-hook
            (quote write-contents-functions)
            (lambda nil
              (delete-trailing-whitespace)
              nil))
           (require
            (quote whitespace))
           "Sometimes the mode needs to be toggled off and on."
           (whitespace-mode 0)
           (whitespace-mode 1))
     (whitespace-line-column . 80)
     (whitespace-style face trailing lines-tail)
     (require-final-newline . t)))))

;; Custom init files
(add-to-list 'load-path "~/.emacs.d/custom")

;; Load custom init files
(load "init-general.el")
(load "init-el-get.el")
(load "init-func.el")
(load "init-ibuffer.el")
(load "init-keys.el")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flx-highlight-face ((t (:inherit font-lock-keyword-face :background "black" :foreground "#FFA319" :weight bold))))
 '(popup-isearch-match ((t (:inherit font-lock-keyword-face :background "black" :foreground "#FFA319"  :weight bold))))
 '(mode-line ((t (:foreground "#030303" :background "#FFA319" :box nil))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))
