(require 'realtime-markdown-viewer)

(setq rtmv:lang 'ruby)

(defun ancane-markdown-replace-table-syntax ()
  (interactive)
  (save-excursion
    (goto-char (point-min))

    (while (re-search-forward "^\\( *-+ *|\\)* *-+$")
      (message (point))
      )
    ))

;; row matching regexp ^[^-|\n]+|\\([^-|\n]*| *\\)*[^-|\n]+$
