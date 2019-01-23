;;; packages.el --- vue layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: lawr <lawrance.rsp@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst vue-packages
  '(
    vue-mode
    flycheck
    lsp
    company-lsp
  ))

(defun vue/init-vue-mode ()
  "Initialize vue package"
  (use-package vue-mode
    :config
    (setq mmm-submode-decoration-level 0)
    ))

(defun vue/post-init-flycheck ()
  (spacemacs/enable-flycheck 'vue-mode))

(defun vue/post-init-lsp ()
    (add-hook 'vue-mode-hook #'lsp))


(defun vue/post-init-company-lsp ()
  (spacemacs|add-company-backends
    :backends company-lsp
    :modes vue-mode))
