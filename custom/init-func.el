(defun kill-and-join-forward (&optional arg)
  "If at end of line, join with following; otherwise kill line. Deletes whitespace at join."
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (delete-indentation t)
    (kill-line arg)))


(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))


(defun back-to-indentation-or-beginning ()
  (interactive)
  (if (or (looking-back "^\s*")
	  (eq last-command 'back-to-indentation-or-beginning))
      (beginning-of-line)
    (back-to-indentation)))


(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))


(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))


(defun lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))


;; Duplicate start of line or region, from http://www.emacswiki.org/emacs/DuplicateStartOfLineOrRegion
(defun duplicate-start-of-line-or-region ()
  (interactive)
  (if mark-active
      (duplicate-region)
    (duplicate-start-of-line)))

(defun duplicate-start-of-line ()
  (if (bolp)
      (progn
        (end-of-line)
        (duplicate-start-of-line)
        (beginning-of-line))
    (let ((text (buffer-substring (point)
                                  (beginning-of-thing 'line))))
      (forward-line)
      (push-mark)
      (insert text)
      (open-line 1))))

(defun duplicate-region ()
  (let* ((end (region-end))
         (text (buffer-substring (region-beginning) end)))
    (goto-char end)
    (insert text)
    (push-mark end)
    (setq deactivate-mark nil)
    (exchange-point-and-mark)))


(setq ancane-search-at-point-wrap nil)

(defun ancane-search-at-point-func (direction)
  (interactive)
  (let* ((text (car search-ring)) newpoint)
    (when ancane-search-at-point-wrap
      (goto-char (if (= direction 1) (point-min) (point-max)))
      (setq ancane-search-at-point-wrap nil))
    (setq newpoint (search-forward text nil t direction))
    (if newpoint
	(set-mark (if (= direction 1) (- newpoint (length text))
		    (+ newpoint (length text))))
      (message "No more: %s" text) (ding)
      (setq ancane-search-at-point-wrap text))))

(defun ancane-search-at-point-forward ()
  (interactive)
  (ancane-search-at-point-func 1))

(defun ancane-search-at-point-backwards ()
  (interactive)
  (ancane-search-at-point-func -1))

(defun yank-thing-into-search ()
  (interactive)
  (let ((text (if mark-active
		  (buffer-substring-no-properties (region-beginning)(region-end))
		(or (current-word) ""))))
    (when (> (length text) 0) (isearch-update-ring text) (setq ancane-search-at-point-wrap nil)
	  (ancane-search-at-point-forward))))

;; New buffer
(defun new-empty-buffer ()
  (interactive)
  (switch-to-buffer (generate-new-buffer "untitled"))
  (funcall (and initial-major-mode))
  (setq buffer-offer-save t)
  )
