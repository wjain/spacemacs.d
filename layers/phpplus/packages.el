(setq phpplus-packages
      '(
        php+-mode
        ))

(defun phpplus/init-php+-mode ()
  (use-package php+-mode
    :defer t
    :mode ("\\.php\\'" . php+-mode)
    :config 
    (progn
      (php+-mode-setup))
    )
  )
