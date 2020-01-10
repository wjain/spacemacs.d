
;;; packages.el --- jain Layer packages File for Spacemacs
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
(setq jain-packages
      '(
        ;; package names go here
        web-mode
        ;; impatient-mode
        ))

;; List of packages to exclude.
(setq jain-excluded-packages '())

;; For each package, define a function jain/init-<package-name>
;;
;; (defun jain/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

;; Fix youdao-dictionary-search `Invalid date:'
(defun url-cookie-expired-p (cookie)
  "Return non-nil if COOKIE is expired."
  (let ((exp (url-cookie-expires cookie)))
    (and (> (length exp) 0)
         (condition-case ()
             (> (float-time) (float-time (date-to-time exp)))
           (error nil)))))

(defun jain/post-init-web-mode ()
  "Initialize my package"
  (if (spacemacs/system-is-mswindows)
      (setq tern-command (cons (executable-find "tern") '()))
    )
  (add-hook 'web-mode-hook 'tern-mode)
  (push '(company-tern) company-backends-web-mode))

(defun jain/init-impatient-mode ()
  "Initialize impatient-mode"
  (use-package impatient-mode
    :defer t
    )
  )
