;;; -*- lexical-binding: t; -*-
;;; sk.el
;;
;; Filename: sk.el
;; Description: A front-end for sk
;; Created: 2017-04-04
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4"))
;;
;; Modified from fzf.el, which is:
;;
;; Copyright (C) 2015 by Bailey Ling
;; Author: Bailey Ling
;; URL: https://github.com/bling/fzf.el
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Install:
;;
;; Autoloads will be set up automatically if you use package.el.
;;
;; Usage:
;;
;; M-x sk
;; M-x sk-directory
;;
;;; Code:

(defgroup sk nil
  "Configuration options for sk.el"
  :group 'convenience)

(defcustom sk/window-height 20
  "The window height of the sk buffer"
  :type 'integer
  :group 'sk)

(defcustom sk/executable "sk"
  "The path to the sk executable."
  :type 'string
  :group 'sk)

(defcustom sk/args "--color 16"
  "Additional arguments to pass into sk."
  :type 'string
  :group 'sk)

(defcustom sk/position-bottom t
  "Set the position of the sk window. Set to nil to position on top."
  :type 'bool
  :group 'sk)

(defun sk/run (args exit-handler)
  "run sk with arguments, call (exit-handler lines) after exit"
  (require 'term)
  (window-configuration-to-register :sk-windows)
  (advice-add 'term-handle-exit :after (sk/wrap-after-term-handle-exit exit-handler))

  (let ((buf (get-buffer-create "*sk*"))
	(window-height (if sk/position-bottom (- sk/window-height) sk/window-height)))
    (split-window-vertically window-height)
    (when sk/position-bottom (other-window 1))
    (if args
	(apply 'make-term "sk" sk/executable nil (split-string args " "))
      (make-term "sk" sk/executable))
    (switch-to-buffer buf)
    (linum-mode 0)
    (set-window-margins nil 1)

    ;; disable various settings known to cause artifacts, see #1 for more details
    (setq-local scroll-margin 0)
    (setq-local scroll-conservatively 0)
    (setq-local term-suppress-hard-newline t) ;for paths wider than the window
    (face-remap-add-relative 'mode-line '(:box nil))

    (term-char-mode)
    (setq mode-line-format (format "   SK  %s" default-directory))))

(defun sk/wrap-after-term-handle-exit (callback)
  (defun handler (process-name msg)
    (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
	   (lines (split-string text "\n" t "\s.*\s"))
	   (pwd default-directory))
      (kill-buffer "*sk*")
      (jump-to-register :sk-windows)
      (let ((default-directory pwd))
	(funcall callback lines))
      (advice-remove 'term-handle-exit #'handler)))
  #'handler)

;;;---------------------------------------------------------------------------- 
;;; find file

(defun sk/callback-find-file (lines)
  (let* ((target (car (last (butlast lines 1))))
	 (file (expand-file-name target)))
    (when (file-exists-p file)
      (find-file file))))

;;;###autoload
(defun sk ()
  "Starts a sk session."
  (interactive)
  (let ((args "")
	(default-directory (condition-case err
			       (projectile-project-root)
			     (error
			      default-directory))))
    (sk/run "" #'sk/callback-find-file)))

;;;###autoload
(defun sk-directory (directory)
  "Starts a sk session at the specified directory."
  (interactive "D")
  (let ((default-directory directory))
    (sk/run "" #'sk/callback-find-file)))

;;;---------------------------------------------------------------------------- 
;;; switch between buffers


(provide 'sk)
;;; sk.el ends here
