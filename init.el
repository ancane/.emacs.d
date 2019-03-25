(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("ELPA"      . "http://tromey.com/elpa/"))
(package-initialize)

;; Custom init files
(add-to-list 'load-path "~/.emacs.d/custom")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(browse-url-browser-function (quote browse-url-chromium))
 '(custom-enabled-themes (quote (atom-one-dark)))
 '(custom-safe-themes
   (quote
    ("08b8807d23c290c840bbb14614a83878529359eaba1805618b3be7d61b0b0a32" "6fc1dd081e496de5ad3e13250c759987e10c71bbf31c102a7dd2b74049d540cb" default)))
 '(custom-theme-directory "~/.emacs.d/custom/themes")
 '(markdown-command "multimarkdown")
 '(markdown-nested-imenu-heading-index t)
 '(nav-boring-file-regexps
   (quote
    ("^[.]$" "~$" "[.]elc$" "[.]pyc$" "[.]o$" "[.]bak$" "^_MTN$" "^blib$" "^CVS$" "^RCS$" "^SCCS$" "^_darcs$" "^_sgbak$" "^autom4te.cache$" "^cover_db$" "^_build$" "^[.]git$" "^[.]ensime")))
 '(nav-width 32)
 '(package-selected-packages
   (quote
    (atom-one-dark-theme inflections queue ido-yes-or-no package-lint web-server let-alist)))
 '(powerline-display-hud nil)
 '(powerline-display-mule-info nil)
 '(powerline-gui-use-vcs-glyph t)
 '(projectile-keymap-prefix (kbd "C-c C-p"))
 '(restclient-print-curl t)
 '(safe-local-variable-values
   (quote
    ((checkdoc-package-keywords-flag)
     (bug-reference-bug-regexp . "#\\(?2:[[:digit:]]+\\)")
     (whitespace-style face tabs trailing lines-tail)
     (eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook"
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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flx-highlight-face ((t (:inherit font-lock-keyword-face :background "yellow" :foreground "black" :weight bold))))
 '(mode-line ((t (:foreground "#030303" :background "#FFA319" :box nil))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil))))
 '(popup-isearch-match ((t (:inherit font-lock-keyword-face :background "yellow" :foreground "black" :weight bold)))))

;; Load custom init files
(load "init-general.el")
(load "init-el-get.el")
(load "init-func.el")
(load "init-ibuffer.el")
(load "init-keys.el")
