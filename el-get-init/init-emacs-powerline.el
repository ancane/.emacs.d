(require 'powerline)

(setq powerline-arrow-shape 'arrow)


(set-face-attribute 'mode-line nil
		    :foreground "#030303"
                    :background "#FFA319"
                    :box nil)

;; (set-face-attribute 'mode-line nil
;;                     :background "OliveDrab3"
;;                     :box nil)

(defpowerline ins-mode (if overwrite-mode "Ovr" "  "))

(setq-default mode-line-format
              (list "%e"
                    '(:eval (concat
                             (powerline-rmw            'left   nil  )
                             (powerline-buffer-id      'left   nil  powerline-color1  )
                             (powerline-major-mode     'left        powerline-color1  )
			     ;; (powerline-minor-modes    'left        powerline-color1  )
                             (powerline-narrow         'left        powerline-color1  powerline-color2  )
                             (powerline-vc             'center                        powerline-color2  )
                             (powerline-make-fill                                     powerline-color2  )
                             (powerline-row            'right       powerline-color1  powerline-color2  )
                             (powerline-make-text      ":"          powerline-color1  )
                             (powerline-column         'right       powerline-color1  )
                             (powerline-percent        'right  nil  powerline-color1  )
			     (powerline-ins-mode       'right  nil  )))))
