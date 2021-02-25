;;; packages.el --- org-roam-server layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author:  <jain.y@DESKTOP-RISE-JA>
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
;; added to `org-roam-server-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `org-roam-server/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `org-roam-server/pre-init-PACKAGE' and/or
;;   `org-roam-server/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst org-roam-server-packages
  '(org-roam-server)
  "The list of Lisp packages required by the org-roam-server layer.

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

(defun org-roam-server/init-org-roam-server ()
 (use-package org-roam-server
     :ensure t
     :config
     (setq org-roam-server-host "127.0.0.1"
           org-roam-server-port 8888
           org-roam-server-authenticate nil
           org-roam-server-export-inline-images t
           org-roam-server-serve-files nil
           org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
           org-roam-server-network-poll t
           org-roam-server-network-arrows nil
           org-roam-server-network-label-truncate t
           org-roam-server-network-label-truncate-length 60
           org-roam-server-network-label-wrap-length 20))
 )
;;; packages.el ends here
