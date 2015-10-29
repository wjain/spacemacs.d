;;; packages.el --- myphp Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq myphp-packages
    '(
      ;; package names go here
      ac-php
      ))

;; List of packages to exclude.
(setq myphp-excluded-packages '())

;; For each package, define a function myphp/init-<package-name>
;;
;; (defun myphp/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun myphp/init-ac-php ()
  "Initialize my package"
  (use-package ac-php
    :defer t
    :init
    (progn
      (require 'cl)
      (require 'php-mode)
      (add-hook 'php-mode-hook
                '(lambda ()
                   (auto-complete-mode t)
                   (require 'ac-php)
                   (setq ac-php-use-cscope-flag  t ) ;;enable cscope
                   (setq ac-sources  '(ac-source-php ) )
                   (yas-global-mode 1)
                   (define-key php-mode-map  (kbd "C-]") 'ac-php-find-symbol-at-point)   ;goto define
                   (define-key php-mode-map  (kbd "C-t") 'ac-php-location-stack-back) ;go back
                   ))
      )
    )
  )
