(global-set-key (kbd "<home>") 'back-to-indentation-or-beginning)
(global-set-key (kbd "<end>") 'end-of-line)

(global-set-key (kbd "C-x C-z") 'ignore)

(global-set-key (kbd "C-k") 'kill-and-join-forward)
(global-set-key (kbd "C-a") 'mark-whole-buffer)

(global-set-key (kbd "<f5>") 'previous-buffer)
(global-set-key (kbd "<f6>") 'next-buffer)

(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

(global-set-key (kbd "M-<f3>")  'yank-thing-into-search)
(global-set-key (kbd "<f3>")    'ancane-search-at-point-forward)
(global-set-key (kbd "C-<f3>")  'ancane-search-at-point-backwards)


(global-set-key (kbd "M-<f4>") 'save-buffers-kill-terminal)
(global-set-key (kbd "M-g") 'goto-line)

(global-set-key (kbd "C-x C-s") 'save-buffer)
(global-set-key (kbd "C-x s")   'save-some-buffers)

(global-set-key (kbd "C-c c") 'ac-start)

(global-set-key (kbd "C-\\")    'comment-or-uncomment-region)
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)


(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "M-3") 'split-window-right)


(global-set-key (kbd "M-S-<down>") 'duplicate-start-of-line-or-region)

;(global-set-key (kbd "<print>") 'helm-mini)

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C-+") 'text-scale-decrease)

(global-set-key (kbd "<f7>") 'start-kbd-macro)
(global-set-key (kbd "<f8>") 'kmacro-end-or-call-macro)

(global-set-key (kbd "C-n") 'new-empty-buffer)
