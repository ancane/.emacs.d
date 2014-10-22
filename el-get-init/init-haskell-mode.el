(require 'haskell-mode)

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def)
(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
;;(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
;;(define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)

;; (custom-set-variables
;;   '(haskell-tags-on-save t))

;; (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)
;;(define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)
