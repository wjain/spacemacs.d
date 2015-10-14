;;; config.el --- jain Layer packages File for Spacemacs
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

(defun init-jain ()
  (config-coding-system)
  (config-misc)
  )

(defun config-coding-system ()
  "Set coding"
  (progn
    (set-language-environment "Japanese")
    (prefer-coding-system 'iso-2022-jp)
    (prefer-coding-system 'shift_jis)
    (prefer-coding-system 'euc-jp)
    (prefer-coding-system 'utf-8)
    (set-default-coding-systems 'shift_jis)
    ;; (set-terminal-coding-system 'shift_jis)
    (set-buffer-file-coding-system 'shift_jis)
    ;; (set-default-coding-systems 'utf-8)
    (set-terminal-coding-system 'utf-8)
    (set-buffer-file-coding-system 'utf-8)
    ;; (prefer-coding-system 'utf-8-unix)
    ;; (set-keyboard-coding-system 'utf-8)
    ;; (setq default-buffer-file-coding-system 'utf-8)
    )
  )

(defun config-misc ()
  "Init misc"
  (progn
    ;; more useful frame title, that show either a file or
    ;; a buffer name (if the buffer isn't visiting a file)
    (setq frame-title-format `((setq my-frame-title-format "emacs@%b %f")))
    )
  )

(init-jain)
