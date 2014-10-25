(defun kill-and-join-forward (&optional arg)
  "If at end of line, join with following; otherwise kill line. Deletes whitespace at join."
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (delete-indentation t)
    (kill-line arg)))


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

(defun duplicate-region (&optional num start end)
  "Duplicates the region bounded by START and END NUM times.
If no START and END is provided, the current region-beginning and
region-end is used."
  (interactive "p")
  (save-excursion
    (let* ((start (or start (region-beginning)))
           (end (or end (region-end)))
           (region (buffer-substring start end)))
      (goto-char end)
      (dotimes (i num)
        (insert region)))))

(defun duplicate-current-line (&optional num)
  "Duplicate the current line NUM times."
  (interactive "p")
  (save-excursion
    (when (eq (point-at-eol) (point-max))
      (goto-char (point-max))
      (newline)
      (forward-char -1))
    (duplicate-region num (point-at-bol) (1+ (point-at-eol)))))


(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated."
  (interactive "p")
  (if (region-active-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (duplicate-region arg beg end)
        (one-shot-keybinding "d" (λ (duplicate-region 1 beg end))))
    (duplicate-current-line arg)
    (one-shot-keybinding "d" 'duplicate-current-line)))

(defun one-shot-keybinding (key command)
  (set-temporary-overlay-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd key) command)
     map) t))



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

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun split-and-goto-window-below ()
  (interactive)
  (split-window-below)
  (other-window 1))

(defun split-and-goto-window-right ()
  (interactive)
  (split-window-right)
  (other-window 1))

(defun just-no-space ()
  (interactive)
  (delete-indentation)
  (delete-horizontal-space))

(defun delete-indentation-one-space ()
  (interactive)
  (just-no-space)
  (just-one-space))

(defun create-scala-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (let* ((working-dir default-directory))
    (setq default-directory dir-name)
    (eshell-command
     (format "find %s -not -path \"./target/*\" -not -path \"./.git/*\" -type f -iname \"*.scala\" | etags --regex=%s -" dir-name (concat "@" (expand-file-name "~") "/etags.scala")))
    (setq default-directory working-dir)
    )
  )

(defun what-position-percentage ()
  (interactive)
  (message
   (number-to-string
    (truncate
     (* 100
        (/
         (float (line-number-at-pos))
         (count-lines (point-min) (point-max))))))))

;;; it's for mark
;; (defvar tmr nil)

;; (setq tmr
;;       (run-with-idle-timer 5 1 'what-position-percentage))

;; (cancel-timer tmr)
;;;;;

;; for scala outline mode

(defun current-etags-to-string ()
  (interactive)
  (message (shell-command-to-string (format "etags -f - --regex=\"/[^\*\\/]*class[ \t]*\\([a-zA-Z0-9_]+\\)/\1/\" --regex=\"/[^\*\\/]*object[ \t]*\\([a-zA-Z0-9_]+\\)/\1/\" --regex=\"/[^\*\\/]*trait[ \t]*\\([a-zA-Z0-9_]+\\)/\1/\" --regex=\"/[^\*\\/]*def[ \t]*\\([^[ \a\b\d\e\f\n\r\t\v(]+\\)[ \t]*.*[:=]/\1/\" --regex=\"/[^\*\\/]*type[ \t]*\\([a-zA-Z0-9_]+\\)[ \t]*[\[<>=]/\1/\" %s" (buffer-file-name)))))
