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

(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (let ((lines (split-string (buffer-string) "\n" t)))
      (if lines lines '()))))

(defun sk/command (opts)
  (let* ((optstr (alist-get 'option opts ""))
	 (interactive? (alist-get 'interactive opts nil))
	 (source (alist-get 'source opts))
	 (source (cond
		  ((and (not source) (getenv "SKIM_DEFAULT_COMMAND"))
		   (let ((temp-file (make-temp-file "sk")))
		     ;; append the default command
		     (write-region (getenv "SKIM_DEFAULT_COMMAND") nil temp-file)
		     (concat "sh " temp-file " 2>/dev/null")))
		  ((and source (stringp) (string= source "none"))
		   nil)
		  (t source)))
	 (prefix (cond
		  (interactive? "")
		  ((not source) "")
		  ((stringp source)
		   (concat source "|"))
		  ((listp source)
		   (let ((temp-file (make-temp-file "sk")))
		     (write-region (mapconcat 'identity source "\n") nil temp-file)
		     (concat "cat " temp-file "|")))
		  (t (throw 'sk/command "invalid source type")))))
    (let ((output-file (make-temp-file "sk")))
      `((command . ,(concat prefix sk/executable " " optstr " > " output-file))
	(output-file . ,output-file)))))


(defun sk/run (opts exit-handler)
  "run sk with arguments, call (exit-handler lines) after exit"
  (require 'term)
  (window-configuration-to-register :sk-windows)

  (let* ((commands (sk/command (if (not (listp opts)) '() opts)))
	 (command (alist-get 'command commands sk/executable))
	 (output-file (alist-get 'output-file commands)))
    (advice-add 'term-handle-exit :after (sk/wrap-after-term-handle-exit exit-handler output-file))
    (let ((buf (get-buffer-create "*SKIM*"))
	  (window-height (if sk/position-bottom (- sk/window-height) sk/window-height)))
      (split-window-vertically window-height)
      (when sk/position-bottom (other-window 1))
      (make-term "SKIM" "sh" nil "-c" command)
      (switch-to-buffer buf)
      (linum-mode 0)
      (set-window-margins nil 1)

      ;; disable various settings known to cause artifacts, see #1 for more details
      (setq-local scroll-margin 0)
      (setq-local scroll-conservatively 0)
      (setq-local term-suppress-hard-newline t) ;for paths wider than the window
      (face-remap-add-relative 'mode-line '(:box nil))

      (term-char-mode)
      (setq mode-line-format (format "   SK  %s" default-directory)))))

(defun sk/wrap-after-term-handle-exit (callback output-file)
  (defun handler (process-name msg)
    (let ((lines (if (file-readable-p output-file)
		     (read-lines output-file)
		   '()))
	  (pwd default-directory))
      (kill-buffer "*SKIM*")
      (jump-to-register :sk-windows)
      (let ((default-directory pwd))
	(funcall callback lines))
      (advice-remove 'term-handle-exit #'handler)))
  #'handler)

;;;----------------------------------------------------------------------------
;;; find file

(defun sk/callback-find-file (lines)
  (when (car lines)
    (let ((file (expand-file-name (car lines))))
      (when (file-exists-p file)
	(find-file file)))))

;;;###autoload
(defun sk ()
  "Starts a sk session."
  (interactive)
  (let ((args "")
	(default-directory (condition-case err
			       (projectile-project-root)
			     (error
			      default-directory))))
    (sk/run '() #'sk/callback-find-file)))

;;;###autoload
(defun sk-directory (directory)
  "Starts a sk session at the specified directory."
  (interactive "D")
  (let ((default-directory directory))
    (sk/run '() #'sk/callback-find-file)))

;;;----------------------------------------------------------------------------
;;; Integration with AG

(defvar ag/file-column-pattern-nogroup
  "^\\(.+?\\):\\([1-9][0-9]*\\):\\([1-9][0-9]*\\):"
  "A regexp pattern that parses `filename:line_num:column_num`")

(defun ag/finished-hook (buffer how-finished)
  "once finished, open the first file."
  (with-current-buffer buffer
    (compile-goto-error)))

(define-compilation-mode ag-mode "SKIM-Ag"
  "Ag results compilation mode"
  (set (make-local-variable 'compilation-error-regexp-alist)
       `((,ag/file-column-pattern-nogroup 1 2)))
  (set (make-local-variable 'compilation-finish-functions)
       #'ag/finished-hook))

(define-key ag-mode-map (kbd "q") '(lambda () (interactive)
				     (let (kill-buffer-query-functions) (kill-buffer))))
(define-key ag-mode-map (kbd "p") #'compilation-previous-error)
(define-key ag-mode-map (kbd "n") #'compilation-next-error)

(defun sk/callback-show-ag-matches (lines)
  (when (car lines)
    (let ((ag-result (make-temp-file "sk")))
      ;; write the lines into a temporary file
      (write-region (mapconcat 'identity lines "\n") nil ag-result)
      (let ((compilation-auto-jump-to-first-error t))
	(compilation-start (concat "cat " ag-result) #'ag-mode)))))

;;;###autoload
(defun ag ()
  "Run SK with ag"
  (interactive)
  (let ((default-directory (condition-case err
			       (projectile-project-root)
			     (error
			      default-directory))))
    (sk/run '((interactive . t)
	      (option . "--ansi -i -m -c 'ag --color --column \"{}\"'"))
	    #'sk/callback-show-ag-matches)))

;;;----------------------------------------------------------------------------
;;; switch between buffers

(provide 'sk)
;;; sk.el ends here
