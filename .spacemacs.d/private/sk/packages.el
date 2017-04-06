;;; packages.el --- sk layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Jinzhou Zhang<lotabout@gmail.com>
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
;; added to `sk-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `sk/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `sk/pre-init-PACKAGE' and/or
;;   `sk/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst sk-packages
  '(compile sk))

(defun sk/init-sk ()
  (use-package sk
    :init
    (use-package compile)
    :config
    (progn
      (define-key evil-normal-state-map (kbd "C-p") 'sk)
      (spacemacs/set-leader-keys "/" 'ag))))

;;; packages.el ends here
