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
  (config-chinese-font)
  )

(defun config-coding-system ()
  "Set coding"
  ;; Japanese
  ;; (set-language-environment "Japanese")
  ;; (prefer-coding-system 'iso-2022-jp)
  ;; (prefer-coding-system 'shift_jis)
  ;; (prefer-coding-system 'euc-jp)
  ;; (prefer-coding-system 'utf-8)
  ;; (set-default-coding-systems 'shift_jis)
  ;; (set-terminal-coding-system 'shift_jis)
  ;; (set-buffer-file-coding-system 'shift_jis)
  ;; (set-default-coding-systems 'utf-8)
  ;; (set-terminal-coding-system 'utf-8)
  ;; (set-buffer-file-coding-system 'utf-8)
  ;; (prefer-coding-system 'utf-8-unix)
  ;; (set-keyboard-coding-system 'utf-8)
  ;; (setq default-buffer-file-coding-system 'utf-8)

  ;; Chinese-GB
  ;; (set-language-environment 'Chinese-GB)
  ;; (set-keyboard-coding-system 'euc-cn)
  ;; (set-clipboard-coding-system 'euc-cn)
  ;; (set-terminal-coding-system 'euc-cn)
  ;; (set-buffer-file-coding-system 'euc-cn)
  ;; (set-selection-coding-system 'euc-cn)
  ;; (modify-coding-system-alist 'process "*" 'euc-cn)
  ;; (setq default-process-coding-system 
  ;;       '(euc-cn . euc-cn))
  ;; (setq-default pathname-coding-system 'euc-cn)
  )

(defun config-misc ()
  "Init misc"
  ;; more useful frame title, that show either a file or
  ;; a buffer name (if the buffer isn't visiting a file)
  (setq frame-title-format `((setq my-frame-title-format "emacs@%b %f")))
  (setq-default ispell-program-name "aspell")
  (ispell-change-dictionary "american" t)
  )

(defun config-chinese-font ()
  (setq solarized-use-variable-pitch nil
        solarized-scale-org-headlines nil)
  (spacemacs/load-theme 'solarized-dark)

  ;; (setq monokai-use-variable-pitch nil
  ;;       monokai-height-plus-1 1
  ;;       monokai-height-plus-2 1
  ;;       monokai-height-plus-3 1
  ;;       monokai-height-plus-4 1
  ;;       monokai-height-minus-1 1
  ;;       )
  ;; (spacemacs/load-theme 'monokai)

  (if (spacemacs/system-is-mac)
      (spacemacs//set-monospaced-font   "Source Code Pro" "华文仿宋" 12 14)
    )

  (if (spacemacs/system-is-mswindows)
      (spacemacs//set-monospaced-font   "Source Code Pro" "新宋体" 12 14)
    )

  (if (spacemacs/system-is-linux)
      ;; ubuntu
      (spacemacs//set-monospaced-font   "Source Code Pro" "WenQuanYi Micro Hei Mono" 12 14)
    )
)


(init-jain)

;; Fix youdao-dictionary-search `Invalid date:'
(defun url-cookie-expired-p (cookie)
  "Return non-nil if COOKIE is expired."
  (let ((exp (url-cookie-expires cookie)))
    (and (> (length exp) 0)
         (condition-case ()
             (> (float-time) (float-time (date-to-time exp)))
           (error nil)))))
