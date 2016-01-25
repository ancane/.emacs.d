
;; Font
(add-to-list 'default-frame-alist '(font . "-xos4-terminus-bold-*-normal-*-18-180-72-72-c-100-koi8-r"))
(set-default-font "-xos4-terminus-bold-*-normal-*-18-180-72-72-c-100-koi8-r")

;; UI settings
(menu-bar-mode   -1)
(tool-bar-mode   -1)
(scroll-bar-mode -1)

(line-number-mode t)
(column-number-mode t)
(global-linum-mode t)
(electric-pair-mode t)
(show-paren-mode t)
(global-auto-revert-mode t)
(global-hl-line-mode t)

(desktop-save-mode t)
(setq desktop-restore-forces-onscreen nil)

(global-subword-mode t)
(random t)
(windmove-default-keybindings 'meta)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

(add-to-list 'default-frame-alist '(alpha . 100))

(setq scroll-step 1
      scroll-conservatively 50
      scroll-margin 5
      scroll-up-margin 5
      scroll-preserve-screen-position t)

(setq inhibit-startup-message t
      redisplay-dont-pause  t
      color-theme-is-global t
      sentence-end-double-space nil
      mouse-yank-at-point t
      uniquify-buffer-name-style 'forward
      whitespace-style '(face trailing lines-tail tabs)
      whitespace-line-column 80
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      diff-switches "-u"
      create-lockfiles nil
      x-select-enable-clipboard t
      fill-column 80
      show-paren-delay 0
      ring-bell-function 'ignore
      desktop-save-buffer t
      require-final-newline t
      windmove-wrap-around t
      default-input-method 'russian-computer
      gc-cons-threshold 20000000
      )

;; UTF-8
(setq locale-coding-system   'utf-8)
(set-language-environment    'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system        'utf-8)

;; (add-hook 'before-save-hook
;;           '(lambda ()
;;              (untabify (point-min) (point-max))
;;              (delete-trailing-whitespace)))

(defalias 'yes-or-no-p 'y-or-n-p)

;; C-c C-v
(cua-mode)

(require 'midnight)

(add-to-list 'auto-mode-alist '("\\.props\\'" . conf-javaprop-mode))
(add-to-list 'auto-mode-alist '("\\.erl\\'" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl\\'" . erlang-mode))



(defun ensime-show-structure-view ()
  "Show source file structure"
  (interactive)
   (let* ((structure (ensime-rpc-structure-view))
          (view (plist-get structure :view))
          (buffer-name ensime-inspector-buffer-name))
     (ensime-with-inspector-buffer
      (buffer-name view t)
      (dolist (item view)
        (insert (format "%s" item))
        (insert "\n")
        ))))
