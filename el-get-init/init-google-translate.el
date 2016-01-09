(require 'google-translate)
(require 'google-translate-default-ui)

(global-set-key (kbd "C-x g t") 'google-translate-at-point)
(global-set-key (kbd "C-x g r") 'google-translate-at-point)
(global-set-key (kbd "C-x C-g t") 'google-translate-query-translate)
(global-set-key (kbd "C-x C-g r") 'google-translate-query-translate-reverse)

(setq google-translate-default-source-language "en")
(setq google-translate-default-target-language "ru")
