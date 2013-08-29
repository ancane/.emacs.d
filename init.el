;; UI settings
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-splash-screen t)
(line-number-mode 1)
(column-number-mode 1)
(scroll-bar-mode -1)
(global-linum-mode 1)

;; Font
(add-to-list 'default-frame-alist  '(font . "-xos4-terminus-bold-*-normal-*-18-180-72-72-c-100-koi8-r"))
(set-default-font "-xos4-terminus-bold-*-normal-*-18-180-72-72-c-100-koi8-r")

;; El-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)


;; C-c C-v
(cua-mode)

;; Clipboard
(setq x-select-enable-clipboard t)

(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)

;; Revert buffer file is changed outside emacs 
;; and contains no unsaved changes
(global-auto-revert-mode 1)

(add-to-list 'default-frame-alist '(alpha . 100))

;; highlits current line
(global-hl-line-mode)
