(require 'nav)
(require 'hl-line)

(global-set-key (kbd "<f12>") 'nav-toggle)
(global-set-key (kbd "C-n")   'nav-toggle)
;(global-set-key (kbd "M-n")   'nav-toggle)

(add-hook 'psw-after-switch-hook 'nav-jump-to-current-dir)

;;;;;;;;;; Lynx-like motion for nav

(defface nav-hl-line
  '((t :background "#777777"))
  "Custom face for highlighting the current line in nav mode."
  :version "22.1"
  :group 'hl-line)


;; This allows global-hl-line be disabled for certain buffers
(make-variable-buffer-local 'global-hl-line-mode)

(defun nav-mode-hl-hook ()
  (setq global-hl-line-mode nil)
  (set (make-local-variable 'hl-line-face) 'nav-hl-line)
  (hl-line-mode t)
  (local-set-key (kbd "<right>") 'nav-open-file-under-cursor)
  (local-set-key (kbd "<left>")  'nav-go-up-one-dir)
  )

(add-hook 'nav-mode-hook 'nav-mode-hl-hook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
