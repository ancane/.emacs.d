
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
   color-theme
   color-theme-tango-2
   ace-jump-mode
   emacs-ido-yes-or-no
   ido-ubiquitous
   js2-mode
   mark-multiple
   smex
   zencoding-mode
   popup-switcher
   expand-region
   emacs-nav
   emacs-powerline ;; TODO replace with original powerline if my pull request is accepted
   auto-complete
   elisp-slime-nav
   paredit
   flx   ;; Ido flex matching
   move-text
   goto-last-change
   old-fashioned-undo
   haskell-mode
   projectile
   web-mode
   markdown-mode
   magit
   less-css-mode
   websocket
   exec-path-from-shell
   realtime-markdown-viewer
   ensime
   ))

(el-get 'sync my:el-get-packages)
