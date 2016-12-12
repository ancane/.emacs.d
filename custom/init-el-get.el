
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

;; Init files
(setq el-get-user-package-directory "~/.emacs.d/el-get-init")

;; El-get from master branch
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

;; Own recipes
(add-to-list 'el-get-recipe-path "~/.emacs.d/custom/recipes")

(setq
 my:el-get-packages
 '(el-get
   s
   dash
   ag
   popup
   websocket
   color-theme
   ace-jump-mode
   emacs-ido-yes-or-no
   ido-ubiquitous
   flx ;; flx-ido
   ido-vertical-mode
   json-reformat
   js2-mode
   restclient
   mark-multiple
   smex
   emmet-mode
   helm
   helm-ag
   emacs-popup-switcher ;;   popup-switcher
   expand-region
   emacs-nav
   emacs-powerline
   emacs-etags-select
   ;;auto-complete
   company-mode
   slime
   elisp-slime-nav
   paredit
   move-text
   goto-last-change
   old-fashioned-undo
   haskell-mode
   projectile
   web-mode
   markdown-mode
   emacs-markdown-preview-mode
   magit
   less-css-mode
   exec-path-from-shell
   scala-mode
   sbt-mode
   org-mode
   google-translate
   emacs-popup-imenu
   clojure-mode
   cider
   yasnippet
   which-key
;;   ensime
;;   emacs-ensime
   )
 )

(el-get 'sync my:el-get-packages)
