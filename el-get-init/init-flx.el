(require 'ido)
(require 'flx-ido)
(require 'recentf)
(require 'ido-ubiquitous)
(require 'ido-yes-or-no)

(ido-mode t)
(ido-ubiquitous t)

(flx-ido-mode t)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)
(setq flx-ido-use-faces nil)

(recentf-mode t)
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
       ido-handle-duplicate-virtual-buffers 2)


(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))


(global-set-key (kbd "C-x C-r") 'ido-recentf-open)
