(require 'powerline)


(setq powerline-arrow-shape 'arrow14)


;; (set-face-attribute 'mode-line nil
;;                     :foreground "#030303"
;;                     :background "#FFA319"
;;                     :box nil)

;; (custom-set-faces
;;  '(mode-line ((t (:foreground "#030303" :background "#bdbdbd" :box nil))))
;;  '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))

(setq powerline-color1 "grey40")
(setq powerline-color2 "black")

(defpowerline ins-mode (if overwrite-mode "Ovr" "   "))


;;(defpowerline project-name (format "[%s]" (projectile-project-name)))


(setq-default
 mode-line-format
 (list "%e"
       '(:eval (append
                (list
                 (powerline-rmw            'right   powerline-color1 powerline-color2  )
                 (powerline-buffer-id      'left   nil  powerline-color1  )
;;                 (powerline-project-name   'left   nil  powerline-color1  )
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
                                       (powerline-ins-mode       'right  nil  )
                                       (powerline-lcl            'right  nil  )
                                       ))
                ))))
