(require 'yasnippet)

(setq yas-snippet-dirs
      '("~/.emacs.d/custom/snippets"                 ;; personal snippets
        "~/.emacs.d/el-get/yasnippet/snippets"
        "~/.emacs.d/el-get/yasnippet/yasmate"
        ))

(yas-global-mode 1)
