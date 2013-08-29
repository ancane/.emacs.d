
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

;; Init files
(setq el-get-user-package-directory "~/.emacs.d/el-get-init")

;(unless (require 'el-get nil 'noerror)
;  (with-current-buffer
;    (url-retrieve-synchronously "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
;    (goto-char (point-max))
;    (eval-print-last-sexp)))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))


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
   ))

(el-get 'sync my:el-get-packages)
