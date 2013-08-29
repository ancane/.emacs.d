(require 'ibuf-ext)

(global-set-key (kbd "C-x o") 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(add-to-list 'ibuffer-never-show-predicates "^\\*")

(setq ibuffer-show-empty-filter-groups nil)

(setq ibuffer-saved-filter-groups
      '(("work"
         ("Scala" (mode . scala-mode))

         ("XHTML" (or (mode . nxhtml-mode)
                     (mode . html-mode)
                     (mode . nxml-mode)
                     (mode . xml-mode)
                     ))

         ("JS"  (or (mode . js2-mode)
                    (mode . js-mode)))

         ("CSS"  (mode . css-mode))

         ("SQL" (mode . sql-mode))

         ("Configs" (or (filename . ".emacs.d")
                        (filename . "el-files")))

         ("Haskell" (mode . haskell-mode))

         ("Org" (or (mode . org-mode)
                    (filename . "OrgMode")))

         ("ELisp" (filename . "el"))

         )))

(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode 1)
             (ibuffer-switch-to-saved-filter-groups "work")))


;; Remove top information bar
(defadvice ibuffer-update-title-and-summary (after remove-column-titles)
  (save-excursion
    (set-buffer "*Ibuffer*")
    (toggle-read-only 0)
    (goto-char 1)
    (search-forward "-\n" nil t)
    (delete-region 1 (point))
    (let ((window-min-height 1))
      ;; save a little screen estate
      (shrink-window-if-larger-than-buffer))
    (toggle-read-only)))

(ad-activate 'ibuffer-update-title-and-summary)


;; Use human readable Size column instead of original one
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000) (format "%7.3fk" (/ (buffer-size) 1000.0)))
   ((> (buffer-size) 1000000) (format "%7.3fM" (/ (buffer-size) 1000000.0)))
   (t (format "%8d" (buffer-size)))))

;; Modify the default ibuffer-formats
(setq ibuffer-formats
      '((mark modified read-only " "
              (name 25 25 :left :elide)
;;              " "
;;              (size-h 9 -1 :right)
              " "
              filename-and-process)))

;; Switching to ibuffer puts the cursor on the most recent buffer
(defadvice ibuffer (around ibuffer-point-to-most-recent) ()
  "Open ibuffer with cursor pointed to most recent buffer name"
  (let ((recent-buffer-name (buffer-name)))
    ad-do-it
    (ibuffer-jump-to-buffer recent-buffer-name)))

(ad-activate 'ibuffer)


;; Fix Backward-Tab when header is disabled.
(progn
  (defun ibuffer-backward-filter-group (&optional count)
    "Move point backwards by COUNT filtering groups."
    (interactive "P")
    (unless count
      (setq count 1))
    (when (> count 0)
      (when (= (point) (point-min))
        (goto-char (point-max))
        (ibuffer-backward-filter-group 1))
      (when (get-text-property (point) 'ibuffer-filter-group-name)
        (goto-char (previous-single-property-change
                    (point) 'ibuffer-filter-group-name
                    nil (point-min))))
      (goto-char (previous-single-property-change
                  (point) 'ibuffer-filter-group-name
                  nil (point-min)))
      (ibuffer-backward-filter-group (1- count)))
    (ibuffer-forward-line 0))
  )
