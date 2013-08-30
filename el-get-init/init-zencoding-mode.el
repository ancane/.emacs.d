(require 'zencoding-mode)

(add-hook 'sgml-mode-hook
          '(lambda () (zencoding-mode 1)))
