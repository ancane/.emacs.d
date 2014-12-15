(require 'powerline)

(setq powerline-arrow-shape 'arrow)


(set-face-attribute 'mode-line nil
                    :foreground "#030303"
                    :background "#FFA319"
                    :box nil)

(defpowerline ins-mode (if overwrite-mode "Ovr" "   "))

(setq-default
 mode-line-format
 (list "%e"
       '(:eval (append
                (list
                 (powerline-lcl            'left   nil  )
                 (powerline-rmw            'left   nil  )
                 (powerline-buffer-id      'left   nil  powerline-color1  )
                 (powerline-major-mode     'left        powerline-color1  )
                 ;;(powerline-minor-modes    'left        powerline-color1  )
                 (powerline-narrow         'left        powerline-color1  powerline-color2  )
                 (powerline-vc             'center                        powerline-color2  ))
                (powerline-pull-right (list
                                       (powerline-row            'right       powerline-color1  powerline-color2  )
                                       (powerline-make-text      ":"          powerline-color1  )
                                       (powerline-column         'right       powerline-color1  )
                                       (powerline-percent        'right  nil  powerline-color1  )
                                       ;; (powerline-display-time   'right  nil)
                                       (powerline-ins-mode       'right  nil  )))
                ))))
