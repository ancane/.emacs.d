(require 'realtime-markdown-viewer)

(setq rtmv:lang 'ruby)

(defun rtmv:send-to-server ()
  (when realtime-markdown-viewer-mode
    (let ((oldbuf (current-buffer)))
      (with-current-buffer (get-buffer-create "*realtime-markdown*")
        (goto-char (point-min))
        (push-mark)
        (goto-char (point-max))
        (kill-region (mark) (point))
        (insert-buffer oldbuf)
        (ancane-markdown-replace-code-syntax)
        (ancane-markdown-replace-table-syntax)
        (websocket-send-text rtmv:websocket (buffer-substring-no-properties (point-min) (point-max)))))))

(defun rtmv:init ()
  (let ((port rtmv:port))
    (rtmv:webapp-launch port)
    (sleep-for 1)
    (rtmv:init-websocket port)
    (add-hook 'kill-emacs-hook 'rtmv:kill-process)
    (add-hook 'after-save-hook 'rtmv:send-to-server nil t)))

(defun ancane-markdown-replace-table-syntax ()
  (goto-char (point-min))
  (while (re-search-forward "^[^.\n|]+|" (point-max) t)
    (beginning-of-line)
    (push-mark)
    (end-of-paragraph-text)
    (let* ((text (buffer-substring (mark) (point)))
           (lines (split-string text "\n")))
      (kill-region (mark) (point))
      (insert
       (concat
        "<table>\n"
        (mapconcat '(lambda (line)
                      (concat
                       "<tr>"
                       (mapconcat '(lambda (cell) (concat "<td>" cell "</td>"))
                                  (split-string line "|")
                                  "\n")
                       "</tr>"))
                   lines
                   "\n")
        "\n</table>")
       )
      )
    )
  )

(defun ancane-markdown-replace-code-syntax ()
  (goto-char (point-min))
  (while (search-forward "```" (point-max) t)
    (delete-backward-char 3)
    (insert "<pre>")
    (search-forward "```" (point-max) t)
    (delete-backward-char 3)
    (insert "</pre>")
    )
  )
