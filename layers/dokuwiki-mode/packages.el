;;; packages.el --- dokuwiki-mode Layer packages File for Spacemacs
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
(setq dokuwiki-mode-packages
    '(
      ;; package names go here
      outline-magic
      dokuwiki-mode
      ))

;; List of packages to exclude.
(setq dokuwiki-mode-excluded-packages '())

;; For each package, define a function dokuwiki-mode/init-<package-name>
;;
;; (defun dokuwiki-mode/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun dokuwiki-mode/init-dokuwiki-mode ()
  (use-package docuwiki-mode
    :defer t
    ;; :init
    ;; (progn
    ;;   (require 'dokuwiki-mode)
    ;;   )
    )
  )

(defun dokuwiki-mode/init-outline-magic ()
  (use-package outline-magic
    :defer t
    :init
    (progn
      (require 'outline-magic)
      )
    )
  )
