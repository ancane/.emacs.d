(require 'ido)
(require 'recentf)

(ido-mode 'both)
(ido-ubiquitous t)
(recentf-mode 1)
(ido-yes-or-no-mode)

(setq recentf-max-saved-items 20)

(setq  ido-auto-merge-work-directories-length nil
       ido-create-new-buffer (quote always)
       ido-enable-flex-matching t
       ido-enable-prefix nil
       ido-everywhere t
       ido-ignore-extensions t
       ido-max-prospects 10
       ido-use-filename-at-point (quote guess)
       ido-use-virtual-buffers t
       ido-handle-duplicate-virtual-buffers 2
       )

(global-set-key (kbd "C-x C-r") 'ido-recentf-open)
