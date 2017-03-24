
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
        nodejs-repl
        php
        ;; company-php
        ;; ac-php
        geben
        codebug
        (doxymacs :location local)
        ;; malabar-mode
        meghanada
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
    :config
    (progn
      (if (spacemacs/system-is-mswindows)
          (progn
            (setq company-c-headers-path-system
                  (quote
                   (
                    "E:/msys64/usr/include/"
                    "E:/msys64/mingw32/include/"
                    "E:/msys64/mingw32/include/c++/5.2.0/"
                    )))
            (setq company-c-headers-path-user
                  (quote
                   ("include/" "header"
                    )))
            )
        )
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

(defun jain/init-nodejs-repl ()
  "Initialize nodejs-repl-mode"
  (use-package nodejs-repl
    :defer t
    )
  )

(defun jain/post-init-php ()
  (when (spacemacs/system-is-mswindows)
    (add-to-list 'load-path "~/.emacs.d/.cache/quelpa/build/php-extras")
    (require 'php-extras)
    )
  )

(defun jain/init-company-php ()
  (add-hook 'php-mode-hook '(lambda ()
                              (company-mode t)
                              (add-to-list 'company-backends 'company-ac-php-backend)
                              (define-key php-mode-map  (kbd "C-M-i") 'company-ac-php-backend)
                              ))
  (use-package company-php
    :defer t
    )
  )

(defun jain/init-ac-php ()
  (add-hook 'php-mode-hook '(lambda ()
                              (require 'ac-php)
                              (define-key php-mode-map  (kbd "C-.") 'ac-php-find-symbol-at-point)   ;goto define
                              (define-key php-mode-map  (kbd "C-,") 'ac-php-location-stack-back   ) ;go back
                              ))
  (use-package ac-php
    :defer t
    )
  )

(defun jain/init-geben ()
  (use-package geben
    :defer t
    )
  )

(defun jain/init-codebug ()
  (use-package codebug
    :defer t
    )
  )

(defun jain/init-doxymacs ()
  "Initialize doxymacs"
  (use-package doxymacs
    :init
    (progn
      (add-hook 'c-mode-common-hook 'doxymacs-mode)
      (add-hook 'php-mode-common-hook 'doxymacs-mode))
    :config
    (progn
      (defun my-doxymacs-font-lock-hook ()
        (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode) (eq major-mode 'php-mode))
            (doxymacs-font-lock)))
      (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
      (spacemacs|hide-lighter doxymacs-mode))))

(defun jain/init-malabar-mode ()
  "Initialize malabar-mode"
  (use-package malabar-mode
    :config
    (progn
      (add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))
      (add-hook 'malabar-mode-hook
                (lambda ()
                  (add-hook 'after-save-hook 'malabar-compile-file-silently
                            nil t)))
      ))
  )

(defun jain/init-meghanada ()
  "Initialize meghanada"
  (use-package meghanada
    :config
    (progn
      (add-hook 'java-mode-hook
                (lambda ()
                  ;; meghanada-mode on
                  (meghanada-mode t)
                  (add-hook 'before-save-hook 'delete-trailing-whitespace)
                  (add-hook 'before-save-hook 'meghanada-code-beautify-befor-save)))
      )))
