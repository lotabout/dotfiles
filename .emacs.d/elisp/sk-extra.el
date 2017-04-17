;;; -*- lexical-binding: t; -*-
;;; sk-extra.el
;;
;; Filename: sk-extra.el
;; Description: Extra plugins for sk
;; Created: 2017-04-04
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4") (sk) (compilation))
;;
;; Copyright (C) 2015 by Jinzhou zhang
;; Author: Jinzhou Zhang
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
;; M-x ag
;; M-x ag-directory
;;
;;; Code:

(require 'sk)
(require 'compile)

;;;----------------------------------------------------------------------------
;;; Integration with AG

(defvar ag/file-column-pattern-nogroup
  "^\\(.+?\\):\\([1-9][0-9]*\\):\\([1-9][0-9]*\\):"
  "A regexp pattern that parses `filename:line_num:column_num`")

(defvar ag-search-finish-hook nil)

(defun ag/finished-hook (buffer how-finished)
  "once finished, open the first file."
  (with-current-buffer buffer
    (compile-goto-error)
    (run-hooks 'ag-search-finish-hook)))

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
      (write-region (mapconcat 'identity lines "\n") nil ag-result nil 'inhibit-message)
      (let ((compilation-auto-jump-to-first-error t))
	(defun on-finish ()
	  (delete-file ag-result)
	  (remove-hook 'ag-search-finish-hook #'on-finish))
	(add-hook 'ag-search-finish-hook #'on-finish)
	(compilation-start (concat "cat " ag-result) #'ag-mode)))))

;;;###autoload
(defun ag ()
  "Run SK with ag"
  (interactive)
  (let ((default-directory (condition-case err
			       (projectile-project-root)
			     (error
			      default-directory))))
    (sk/run `((interactive . t)
	      (option . ,(concat "--ansi -i -m -c '" sk/ag-command " \"{}\"'")))
	    #'sk/callback-show-ag-matches)))
;;;###autoload
(defun ag-directory (directory)
  "Starts a sk session at the specified directory."
  (interactive "D")
  (let ((default-directory directory))
    (sk/run `((interactive . t)
	      (option . ,(concat "--ansi -i -m -c '" sk/ag-command " \"{}\"'")))
	    #'sk/callback-show-ag-matches)))

(provide 'sk-extra)
;;; sk-extra.el ends here
