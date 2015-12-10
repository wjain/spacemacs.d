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
        ;; company-c-headers
        web-mode
        impatient-mode
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

(defun jain/post-init-company-c-headers()
  (use-package company-c-headers
    :defer t
    :init
    (progn
      (setq company-c-headers-path-system
            (quote
             (
              "E:/msys32/usr/include/"
              "E:/msys32/mingw32/include/"
              "E:/msys32/mingw32/include/c++/5.2.0/"
              )))
      (setq company-c-headers-path-user
            (quote
             ("include/" "header"
              )))
      )
    ))

;; Fix youdao-dictionary-search `Invalid date:'
(defun url-cookie-expired-p (cookie)
  "Return non-nil if COOKIE is expired."
  (let ((exp (url-cookie-expires cookie)))
    (and (> (length exp) 0)
         (condition-case ()
             (> (float-time) (float-time (date-to-time exp)))
           (error nil)))))

(defun jain/init-web-mode ()
  "Initialize my package"
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (add-hook 'js-mode-hook 'tern-mode)
  (if (spacemacs/system-is-mswindows)
      (setq tern-command (cons (executable-find "tern") '()))
    )
  )

(defun jain/init-impatient-mode ()
  "Initialize impatient-mode"
  (use-package impatient-mode
    :defer t
    )
  )
