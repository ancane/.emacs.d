
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("ELPA"      . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("melpa"     . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;; UI settings
(menu-bar-mode   -1)
(tool-bar-mode   -1)
(scroll-bar-mode -1)

(line-number-mode   1)
(column-number-mode 1)
(global-linum-mode  1)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

(setq inhibit-splash-screen t)
(setq redisplay-dont-pause  t)

(setq scroll-step 1
      scroll-conservatively 50
      scroll-margin 5
      scroll-up-margin 5
      scroll-preserve-screen-position t)

(setq inhibit-startup-message t
      color-theme-is-global t
      sentence-end-double-space nil
      mouse-yank-at-point t
      uniquify-buffer-name-style 'forward
      whitespace-style '(face trailing lines-tail tabs)
      whitespace-line-column 80
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      diff-switches "-u")

;; Font
(add-to-list 'default-frame-alist '(font . "-xos4-terminus-bold-*-normal-*-18-180-72-72-c-100-koi8-r"))
(set-default-font "-xos4-terminus-bold-*-normal-*-18-180-72-72-c-100-koi8-r")

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

;; UTF-8
(setq locale-coding-system   'utf-8)
(set-language-environment    'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system        'utf-8)

(setq default-input-method 'russian-computer)

(add-hook 'before-save-hook
          '(lambda () (delete-trailing-whitespace)))

(show-paren-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Seed the random-number generator
(random t)

;; GC tuning: start after every new 20Mb allocated
(setq gc-cons-threshold 20000000)

(setq fill-column 80)
