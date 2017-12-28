;;; packages.el --- rtags layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author:  <jain.y@LAPTOP-Jain>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `rtags-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `rtags/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `rtags/pre-init-PACKAGE' and/or
;;   `rtags/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst rtags-packages
  '(
    company-rtags
    rtags
    )
  "The list of Lisp packages required by the rtags layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")


;;; packages.el ends here
(defun rtags-evil-standard-keybindings (mode)
  (evil-leader/set-key-for-mode mode
    "mR." 'rtags-find-symbol-at-point
    "mR," 'rtags-find-references-at-point
    "mRv" 'rtags-find-virtuals-at-point
    "mRV" 'rtags-print-enum-value-at-point
    "mR/" 'rtags-find-all-references-at-point
    "mRY" 'rtags-cycle-overlays-on-screen
    "mR>" 'rtags-find-symbol
    "mR<" 'rtags-find-references
    "mR[" 'rtags-location-stack-back
    "mR]" 'rtags-location-stack-forward
    "mRD" 'rtags-diagnostics
    "mRG" 'rtags-guess-function-at-point
    "mRp" 'rtags-set-current-project
    "mRP" 'rtags-print-dependencies
    "mRe" 'rtags-reparse-file
    "mRE" 'rtags-preprocess-file
    "mRR" 'rtags-rename-symbol
    "mRM" 'rtags-symbol-info
    "mRS" 'rtags-display-summary
    "mRO" 'rtags-goto-offset
    "mR;" 'rtags-find-file
    "mRF" 'rtags-fixit
    "mRL" 'rtags-copy-and-print-current-location
    "mRX" 'rtags-fix-fixit-at-point
    "mRB" 'rtags-show-rtags-buffer
    "mRI" 'rtags-imenu
    "mRT" 'rtags-taglist
    "mRh" 'rtags-print-class-hierarchy
    "mRa" 'rtags-print-source-arguments
    )
  )

(defun rtags/init-company-rtags ()
  "Initialize my package"
  (use-package company-rtags
    ))


;; For each package, define a function rtags/init-<package-name>
;;
(defun rtags/init-rtags ()
  "Initialize my package"
  (use-package rtags
    :init
    ;;(evil-set-initial-state 'rtags-mode 'emacs)
    ;;(rtags-enable-standard-keybindings c-mode-base-map)
    :ensure company
    :config
    (progn
      (require 'company-rtags)
      (add-to-list 'company-backends 'company-rtags)
      (setq company-rtags-begin-after-member-access t)
      (setq rtags-completions-enabled t)
      ;;(rtags-diagnostics)
      (define-key evil-normal-state-map (kbd "RET") 'rtags-select-other-window)
      (define-key evil-normal-state-map (kbd "M-RET") 'rtags-select)
      (define-key evil-normal-state-map (kbd "q") 'rtags-bury-or-delete)

      (rtags-evil-standard-keybindings 'c-mode)
      (rtags-evil-standard-keybindings 'c++-mode)
      )
    )
  )
