
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
   ido-yes-or-no
   ido-ubiquitous
   flx
   ido-vertical-mode
   json-reformat
   restclient
   mark-multiple
   smex
   popup-switcher
   expand-region
   emacs-nav
   emacs-powerline
   company-mode
   slime
   elisp-slime-nav
   paredit
   move-text
   goto-last-change
   old-fashioned-undo
   projectile
   web-mode
   markdown-mode
   markdown-preview-mode
   git-commit-mode
   magit
   scala-mode
   sbt-mode
   org-mode
   google-translate
   emacs-popup-imenu
   clojure-mode
   cider
   clj-refactor
   clojure-snippets
   yasnippet
   which-key
   )
 )

(el-get 'sync my:el-get-packages)
