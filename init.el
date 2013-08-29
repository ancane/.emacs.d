;; UI settings
(menu-bar-mode   -1)
(tool-bar-mode   -1)
(scroll-bar-mode -1)

(line-number-mode   1)
(column-number-mode 1)
(global-linum-mode  1)

(setq inhibit-splash-screen t)
(setq redisplay-dont-pause  t)

(setq scroll-step 1
      scroll-conservatively 50
      scroll-margin 5
      scroll-up-margin 5
      scroll-preserve-screen-position t)

;; Font
(add-to-list 'default-frame-alist '(font . "-xos4-terminus-bold-*-normal-*-18-180-72-72-c-100-koi8-r"))
(set-default-font "-xos4-terminus-bold-*-normal-*-18-180-72-72-c-100-koi8-r")

;; Custom init files
(add-to-list 'load-path "~/.emacs.d/custom")

;; El-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

;; Init files
(setq el-get-user-package-directory "~/.emacs.d/el-get-init")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer 
    (url-retrieve-synchronously "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))


(setq
 my:el-get-packages
 '(el-get
   ace-jump-mode
   color-theme
   color-theme-tango-2
   ))

(el-get 'sync my:el-get-packages)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; Highlits current line
(global-hl-line-mode)

;; No ring or visual warnings
(setq ring-bell-function 'ignore)

;; Save desktop
(desktop-save-mode t)
(setq desktop-save-buffer t)

(setq require-final-newline 't)

;; For camelCase parts selection
(global-subword-mode 1)

;; Использовать окружение UTF-8
(set-language-environment 'UTF-8)
;; UTF-8 для вывода на экран
(set-terminal-coding-system 'utf-8)
;; UTF-8 для ввода с клавиатуры
(set-keyboard-coding-system 'utf-8)
;; Необходима поддержка кодировок cp866 и cp1251
(define-coding-system-alias 'windows-1251 'cp1251)
;; Установки автоопределения кодировок
;; prefer-coding-system помещает кодировку в НАЧАЛО списка предпочитаемых кодировок
;; Поэтому в данном случае первой будет определяться utf-8-unix
(prefer-coding-system 'cp866)
(prefer-coding-system 'koi8-r-unix)
(prefer-coding-system 'windows-1251-dos)
(prefer-coding-system 'utf-8-unix)

(setq default-input-method 'russian-computer)

;; Load custom init files
(load "init-ibuffer.el")


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
