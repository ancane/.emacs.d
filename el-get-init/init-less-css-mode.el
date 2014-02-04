(require 'less-css-mode)

(defcustom less-css-default-output-directory nil
  "Default directory in which to save CSS, or nil to use the LESS file's directory.

This path is expanded relative to the directory of the LESS file
using `expand-file-name', so both relative and absolute paths
will work as expected."
  :type 'string
  :group 'less-css)

(defun less-css--output-path ()
  "Calculate the path for the compiled CSS file created by `less-css-compile'."
  (expand-file-name (or less-css-output-file-name
                        (concat (file-name-nondirectory (file-name-sans-extension buffer-file-name)) ".css"))
                    (or less-css-default-output-directory less-css-output-directory default-directory)))

(setq less-css-compile-at-save t)
(setq less-css-lessc-options '("--no-color --compress"))
(setq less-css-default-output-directory "../css")
