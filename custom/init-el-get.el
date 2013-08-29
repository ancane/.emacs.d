
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
   ido-yes-or-no
   js2-mode
   mark-multiple
   ido-ubiquitous
   smex
   zencoding-mode
   ensime
   scala-mode2
   popup-switcher
   emacs-nav
   ))

(el-get 'sync my:el-get-packages)
