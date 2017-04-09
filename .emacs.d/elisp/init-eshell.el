;;;-----------------------------------------------------------------------------
;;; settings

(setq eshell-scroll-to-bottom-on-input 'all
      eshell-error-if-no-glob t
      eshell-hist-ignoredups t
      eshell-save-history-on-exit t
      eshell-prefer-lisp-functions nil)

(add-hook 'eshell-mode-hook
          (lambda ()
            (add-to-list 'eshell-visual-commands "ssh")
            (add-to-list 'eshell-visual-commands "tail")))

;;;-----------------------------------------------------------------------------
;;; Prompt
;;; Borrowed from
;;; http://blog.liangzan.net/blog/2012/12/12/customizing-your-emacs-eshell-prompt/

(defun curr-dir-git-branch-string (pwd)
  "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
  (interactive)
  (when (and (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let ((git-output (shell-command-to-string (concat "cd " pwd " && (command git symbolic-ref HEAD 2>/dev/null || command git rev-parse --short HEAD 2> /dev/null) | sed 's:refs/heads/::'"))))
      (concat " (" (substring git-output 0 -1) ")"))))

;; handle PATH
(defun pwd-repl-home (pwd)
  (interactive)
  (let* ((home (expand-file-name (getenv "HOME")))
         (home-len (length home)))
    (if (and
         (>= (length pwd) home-len)
         (equal home (substring pwd 0 home-len)))
        (concat "~" (substring pwd home-len))
      pwd)))

(setq eshell-prompt-function
      (lambda ()
        (concat
         ((lambda (p-lst)
            (if (> (length p-lst) 3)
                (concat
                 (mapconcat (lambda (elm)
                              (if (zerop (length elm)) "" (substring elm 0 1)))
                            (butlast p-lst 3)
                            "/")
                 "/"
                 (mapconcat (lambda (elm) elm)
                            (last p-lst 3)
                            "/"))
              (mapconcat (lambda (elm) elm)
                         p-lst
                         "/")))
          (split-string (pwd-repl-home (eshell/pwd)) "/"))
         (or (curr-dir-git-branch-string (eshell/pwd)))
         (if (= (user-uid) 0) " # " " $ "))))

;;;-----------------------------------------------------------------------------
;;; Handy ehsell functions

(defun eshell/clear ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (eshell-send-input))

(defun eshell/emacs (&rest args)
  "Open a file in emacs. Some habits die hard."
  (if (null args)
      ;; If I just ran "emacs", I probably expect to be launching
      ;; Emacs, which is rather silly since I'm already in Emacs.
      ;; So just pretend to do what I ask.
      (bury-buffer)
    ;; We have to expand the file names or else naming a directory in an
    ;; argument causes later arguments to be looked for in that directory,
    ;; not the starting directory
    (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))))

(defalias 'ec 'eshell/emacs)

(defun eshell/vi (&rest args)
  "Invoke `find-file' on the file.
    \"vi +42 foo\" also goes to line 42 in the buffer."
  (while args
    (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
	(let* ((line (string-to-number (match-string 1 (pop args))))
	       (file (pop args)))
	  (find-file file)
	  (goto-line line))
      (find-file (pop args)))))

(defun eshell/j ()
  "Invoke fasd and navigate to corresponding directory"
  (defun callback (lines)
    (save-excursion
      (when (car lines)
        (eshell/cd (car lines))
        (eshell-send-input))))
  (sk/run `((source . ,(split-string (shell-command-to-string "fasd -Rdl") "\n")))
          #'callback))

(provide 'init-eshell)
;;; init-eshell ends here
