(require 'google-translate)
(require 'google-translate-default-ui)

(global-set-key "\C-ct" 'google-translate-at-point)
(global-set-key "\C-cT" 'google-translate-query-translate)

(setq google-translate-default-source-language "en")
(setq google-translate-default-target-language "ru")
