;;; packages.el --- irony Layer packages File for Spacemacs
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
(setq irony-packages
      '(
        ;; package names go here
        irony
        company-irony
        company-irony-c-headers
        ))

;; List of packages to exclude.
(setq irony-excluded-packages '())

;; For each package, define a function irony/init-<package-name>
;;
;; (defun irony/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun irony/init-irony ()
  (use-package irony
    :defer t
    :init
    (progn
      (add-hook 'c++-mode-hook 'irony-mode)
      (add-hook 'c-mode-hook 'irony-mode)
      (add-hook 'objc-mode-hook 'irony-mode)

      ;; replace the `completion-at-point' and `complete-symbol' bindings in
      ;; irony-mode's buffers by irony-mode's function
      (defun my-irony-mode-hook ()
        (define-key irony-mode-map [remap completion-at-point]
          'irony-completion-at-point-async)
        (define-key irony-mode-map [remap complete-symbol]
          'irony-completion-at-point-async))
      (add-hook 'irony-mode-hook 'my-irony-mode-hook)
      ;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

      (setq w32-pipe-read-delay 0)
      )
    )
  )

(defun irony/init-company-irony ()
  (use-package company-irony
    :defer t
    :init
    (progn
      (eval-after-load 'company
        '(add-to-list 'company-backends 'company-irony)))
    )
  )

(defun irony/init-company-irony-c-headers ()
  (use-package company-irony-c-headers
    :defer t
    :init
    (progn
      ;; Load with `irony-mode` as a grouped backend
      (eval-after-load 'company
        '(add-to-list
          'company-backends '(company-irony-c-headers company-irony))))
   )
  )
