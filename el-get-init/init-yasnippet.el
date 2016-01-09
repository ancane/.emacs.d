(require 'yasnippet)

(setq yas-snippet-dirs
      '("~/.emacs.d/custom/snippets"                 ;; personal snippets
        "/path/to/some/collection/"           ;; foo-mode and bar-mode snippet collection
        "/path/to/yasnippet/yasmate/snippets" ;; the yasmate collection
        "/path/to/yasnippet/snippets"         ;; the default collection
        ))

(yas-global-mode 1)
