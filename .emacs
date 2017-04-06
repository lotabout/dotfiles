;==============================================================================
;;; General settings

;;; package archive
(when (>= emacs-major-version 24)
  (setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
			   ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
			   ("org" . "http://orgmode.org/elpa/")))
  (package-initialize))

;;; custom scripts
(add-to-list 'load-path "~/.emacs.d/elisp")

;;; load use-package for package management
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
(require 'use-package)
(use-package diminish
  :ensure t)

;;; color theme
(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

;------------------------------------------------------------------------------
;;; general settings

;;; disable menu bar
(menu-bar-mode -1)

;;; disable toolbar and scrollbar on GUI mode
(when window-system
  (progn
    (tool-bar-mode -1)
    (scroll-bar-mode -1)))

; enable highlight line mode
(add-hook 'find-file-hook
	  (lambda ()
	    (hl-line-mode 1)
	    ;; TODO: get color from color theme
	    (set-face-background 'hl-line "gray25")))

;;; Show matched parents
(show-paren-mode t)

;;; turn on auto-fill mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;; disable splash screen and minibuffer messages on startup
(setq inhibit-splash-screen t)
(setq inhibit-startup-echo-area-message t)

(setq ring-bell-function 'ignore)

(setq default-major-mode 'text-mode)

;;; backup directory
(setq backup-directory-alist `(("." . "~/.emacs-backup"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;;; OS specific default.
(cond
 ((string-equal system-type "darwin")
  (progn
    ;; Use Meta as Alt in OSX
    (customize-set-variable 'ns-command-modifier 'meta)

    ;; set up PATH
    (use-package exec-path-from-shell
      :ensure t
      :config
      (exec-path-from-shell-initialize))

    (set-default-font "Source Code Pro-14")
    ))
 ((string-equal system-type "gnu/linux")
  (progn
    (when window-system
      (progn
	;; English Font
	(set-default-font "DejaVu Sans Mono-11")
	;; Chinese Font
	(dolist (charset '(kana han symbol cjk-misc bopomofo))
	  (set-fontset-font (frame-parameter nil 'font)
			    charset
			    (font-spec :family "WenQuanYi Micro Hei Mono")))
	(setq face-font-rescale-alist '(("WenQuanYi Micro Hei Mono" . 1.2))))))))

;; turn on save place so that when opening a file, the cursor will be at the last position.
(if (>= emacs-major-version 25)
    (save-place-mode 1)
  (progn
      (require 'saveplace)
      (setq-default save-place t)))

;;; scroll by one line.
(setq scroll-conservatively 1)
(setq scroll-step 1)

;;; Utilities
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    (declare (indent 1) (debug t))
    `(eval-after-load ,file '(progn ,@body))))

(require 'utility-functions)

;-----------------------------------------------------------------------------
; Global key bindings

;; move window
(global-set-key (kbd "M-k") 'windmove-up)
(global-set-key (kbd "M-j") 'windmove-down)
(global-set-key (kbd "M-l") 'windmove-right)
(global-set-key (kbd "M-h") 'windmove-left)

(define-key global-map (kbd "RET") 'newline-and-indent)

;;;----------------------------------------------------------------------------
;;; evil -- emulation of vim

(use-package evil
  :ensure t
  :init
  (progn
    ;; if we don't have this evil overwrites the cursor color
    (setq evil-default-cursor t)

    ;; evil-leader
    (use-package evil-leader
      :ensure t
      :init (global-evil-leader-mode)
      :config
      (progn
	(setq evil-leader/in-all-states t)

	(evil-leader/set-leader "SPC")

	;; key bindings

	(evil-leader/set-key
	  "l" 'ace-jump-line-mode

	  "<SPC>" 'delete-trailing-whitespace

	  ;; remove Ctrl-M
	  "m" '(lambda () (interactive) (region-replace "" ""))

	  ;; collapse blank lines
	  "<RET>" '(lambda () (interactive) (region-replace "^\n\\{2,\\}" "\n"))

	  ;; neotree
	  "ne" 'neotree-toggle
	  "nf" 'neotree-find

	  )))

    ;; enable evil by default
    (evil-mode 1))

  :config
  (progn
     (global-evil-matchit-mode 1)
     (global-evil-visualstar-mode 1)
     (global-evil-search-highlight-persist t)
     (customize-set-variable 'evil-toggle-key "C-`")

     ;; disable evil mode for some major modes
     (evil-set-initial-state 'calendar-mode 'emacs)
     (evil-set-initial-state 'term-mode 'emacs)

     ;; key bindings
     (define-key evil-insert-state-map (kbd "C-a") nil)
     (define-key evil-insert-state-map (kbd "C-d") nil)
     (define-key evil-insert-state-map (kbd "C-e") nil)
     (define-key evil-insert-state-map (kbd "C-k") nil)
     (define-key evil-insert-state-map (kbd "C-y") nil)

     (define-key evil-normal-state-map (kbd "[b") 'evil-prev-buffer)
     (define-key evil-normal-state-map (kbd "]b") 'evil-next-buffer)

     (define-key evil-normal-state-map "gp" 'evil-paste-select)

     ;; evil-number
     (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
     (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)

     ;; clear screen and highlight
     (define-key evil-normal-state-map (kbd "C-d") '(lambda ()
						      (interactive)
						      (evil-search-highlight-persist-remove-all)
						      (redraw-frame)))

     (define-key evil-normal-state-map (kbd "f") 'ace-jump-char-mode)

    ;; disable evil in these modes
    (setq evil-emacs-state-modes
	  (append evil-emacs-state-modes
		  '(cider-repl-mode
		    cider-stacktrace-mode
		    eclim-project-mode)))
     ))

;;; helper functions

(defun evil-paste-select ()
  (interactive)
  (let ((begin (nth 3 evil-last-paste))
	(end (- (nth 4 evil-last-paste) 1)))
    (evil-visual-select begin end)))

(use-package evil-visualstar
  :ensure t
  :defer t)

(use-package evil-nerd-commenter
  :ensure t
  :commands evilnc-comment-or-uncomment-lines)

(use-package evil-numbers
  :ensure t
  :defer t)

(use-package evil-paredit
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
    (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
    (add-hook 'scheme-mode-hook 'paredit-mode)
    (add-hook 'scheme-mode-hook 'evil-paredit-mode)
    (add-hook 'clojure-mode-hook 'paredit-mode)
    (add-hook 'clojure-mode-hook 'evil-paredit-mode)))

(use-package evil-search-highlight-persist
  :ensure t
  :defer t)

(use-package evil-matchit
  :ensure t
  :defer t)

;;;----------------------------------------------------------------------------
;;; ace-jump -- i.e easymotion for vim
(use-package ace-jump-mode
  :ensure t
  :commands ac-jump-mode)

;;;----------------------------------------------------------------------------
;;; smex
(use-package smex
  :ensure t
  :config (smex-initialize)
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands)) )

;;;----------------------------------------------------------------------------
;;; undo tree
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (progn
    (setq undo-tree-auto-save-history t
         undo-tree-history-directory-alist
         `(("." . "~/.emacs-undo")))
    (unless (file-exists-p "~/.emacs-undo")
      (make-directory "~/.emacs-undo"))))

;;;----------------------------------------------------------------------------
;;; persp-mode / perspective
(use-package persp-mode
  :ensure t
  :commands persp-mode
  :init
  (progn
    (setq persp-autokill-buffer-on-remove 'kill-weak)
    (customize-set-variable 'persp-keymap-prefix (kbd "C-SPC"))
    (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))
  :config
  (progn
    (setq wg-morph-on nil) ;; switch off animation of restoring window configuration

    (customize-set-variable 'persp-nil-name "main")
    (customize-set-variable 'persp-auto-save-opt 0) ; disable auto-save
    (customize-set-variable 'persp-auto-resume-time 0)	; disable auto-resume

    (define-key persp-key-map (kbd "c") #'persp-switch)
    (define-key persp-key-map (kbd "n") #'persp-switch)
    (define-key persp-key-map (kbd "p") #'persp-switch)
    (define-key persp-key-map (kbd "r") #'persp-rename)
    (define-key persp-key-map (kbd "k") #'persp-kill)
    (define-key persp-key-map (kbd "a") #'persp-add-buffer)
    (define-key persp-key-map (kbd "t") #'persp-temporarily-display-buffer)
    (define-key persp-key-map (kbd "i") #'persp-import-buffers)
    (define-key persp-key-map (kbd "q") #'persp-remove-buffer)
    (define-key persp-key-map (kbd "w") #'persp-save-state-to-file)
    (define-key persp-key-map (kbd "l") #'persp-load-state-from-file)
    (define-key persp-key-map (kbd "SPC") #'zoom-window-zoom)))

(use-package zoom-window
  :ensure t
  :commands zoom-window-zoom
  :config
  (progn

    (custom-set-variables '(zoom-window-use-persp t)
			  '(zoom-window-mode-line-color "cyan4"))))

;;;----------------------------------------------------------------------------
;;; NeoTree -- NERD-tree for emacs
(use-package neotree
  :ensure t
  :commands (neotree-toggle neotree-find)
  :bind ("<f8>" . neotree-toggle)
  :config
  (add-hook 'neotree-mode-hook
	    (lambda ()
	      (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
	      (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
	      (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
	      (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
	      (define-key evil-normal-state-local-map (kbd "o") 'neotree-enter)
	      (define-key evil-normal-state-local-map (kbd "j") 'neotree-next-line)
	      (define-key evil-normal-state-local-map (kbd "k") 'neotree-previous-line)
	      (define-key evil-normal-state-local-map (kbd "M") 'neotree-create-node)
	      (define-key evil-normal-state-local-map (kbd "R") 'neotree-rename-node)
	      (define-key evil-normal-state-local-map (kbd "D") 'neotree-delete-node)
	      (define-key evil-normal-state-local-map (kbd "r") 'neotree-refresh)
	      (define-key evil-normal-state-local-map (kbd "C") 'neotree-change-root)
	      (define-key evil-normal-state-local-map (kbd "U") 'neotree-change-root)
	      (define-key evil-normal-state-local-map (kbd "O") 'neotree-open-directory-recursively)
	      (define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle))))

(defun neotree-expand-node-descendants (&optional arg)
  "Expand the line under the cursor and all descendants.
Optional argument ARG indicates that any cache should be flushed."
  (interactive "P")
  (let ((full-path (if arg arg (neo-buffer--get-filename-current-line))))
    (when (file-directory-p full-path)
      (neo-buffer--set-expand full-path t)
      ; recursive expand the nodes;
      (dolist (node (car (neo-buffer--get-nodes full-path)))
        (neotree-expand-node-descendants node)))))

(defun neotree-open-directory-recursively (&optional arg)
  "Expand a directory recursively"
  (interactive "P")
  (neotree-expand-node-descendants arg)
  (neo-buffer--refresh t))

;;;----------------------------------------------------------------------------
;;; projectile
(use-package projectile
  :ensure t
  :init
  (customize-set-variable 'projectile-keymap-prefix "")
  :config
  (projectile-global-mode t)
  :diminish projectile-mode)

;;;----------------------------------------------------------------------------
;;; multi-eshell
(use-package multi-eshell
  :ensure t
  :bind ("\C-ce" . get-eshell)
  :init
  (progn
    (setq multi-eshell-shell-function '(eshell))
    (setq multi-eshell-name "*eshell*"))
  :config
  (progn
    (use-package eshell-functions)
    (add-hook 'eshell-mode-hook
	      '(lambda()
		 (local-set-key "\C-cn" 'multi-eshell-switch-to-next-live-shell)
		 (local-set-key "\C-cp" 'multi-eshell-switch)))))

; shell completion
(defun last-eshell-buffer (l)
  "Return most recently used eshell buffer."
  (when l
    (if (eq 'eshell-mode (with-current-buffer (car l) major-mode))
	(car l) (last-eshell-buffer (cdr l)))))

(defun get-eshell()
  "Switch to the eshell buffer last used, or create a new one if non exists, or if the current buffer is already a eshell"
  (interactive)
  (let ((b (last-eshell-buffer
	    (if (fboundp 'with-persp-buffer-list)
		(with-persp-buffer-list () (buffer-list))
	      (buffer-list)))))
    (if (or (not b) (eq 'eshell-mode major-mode))
	(progn (multi-eshell 1)
	       (when (fboundp 'persp-add-buffer)
		 (persp-add-buffer (current-buffer))))
      (switch-to-buffer b))))

;;;----------------------------------------------------------------------------
;;; multi-term
(use-package multi-term
  :ensure t
  :bind ("\C-ct" . get-term)
  :init
  (progn
    (setq multi-term-program "/bin/zsh")
    (setq term-unbind-key-list '("C-SPC" "C-z" "C-x" "C-c" "C-h" "C-y" "M-x" "M-:")))

  :config
  (progn
     (add-to-list 'term-bind-key-alist '("C-c n" . multi-term-next))
     (add-to-list 'term-bind-key-alist '("C-c p" . multi-term-prev))))

(defun last-term-buffer (l)
  "Return most recently used term buffer."
  (when l
    (if (eq 'term-mode (with-current-buffer (car l) major-mode))
	(car l)(last-term-buffer (cdr l)))))

(defun get-term()
  "Switch to the term buffer last used, or create a new one if non exists, or if the current buffer is already a term"
  (interactive)
  (let ((b (last-term-buffer (buffer-list))))
    (if (or (not b) (eq 'term-mode major-mode))
	(multi-term)
      (switch-to-buffer b))))

(defun get-term()
  "Switch to the term buffer last used, or create a new one if non exists, or if the current buffer is already a term"
  (interactive)
  (let ((b (last-term-buffer
	    (if (fboundp 'with-persp-buffer-list)
		(with-persp-buffer-list () (buffer-list))
	      (buffer-list)))))
    (if (or (not b) (eq 'term-mode major-mode))
	(progn (multi-term)
	       (when (fboundp 'persp-add-buffer)
		 (persp-add-buffer (current-buffer))))
      (switch-to-buffer b))))

;;;----------------------------------------------------------------------------
;;; which key
(use-package which-key
  :ensure t
  :init
  (which-key-mode))

;;;----------------------------------------------------------------------------
;;; yasnippet
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :commands (yas-global-mode yas-minor-mode)
  :init
  (progn
    ;; we don't want undefined variable error
    (defvar yas-global-mode nil)
    (setq yas-triggers-in-field t
	  yas-wrap-around-region t)

    ;; enable yasnippet for several minor modes
    (add-hook 'prog-mode-hook #'yas-minor-mode)
    (add-hook 'org-mode-hook #'yas-minor-mode))
  :config
  (progn
    (yas-reload-all)

    ;; disable `yas-expand` binding to "TAB" key. trigger by company mode instead.
    (define-key yas-minor-mode-map (kbd "TAB") nil)
    (define-key yas-keymap (kbd "C-j") 'yas-next-field-or-maybe-expand)
    (define-key yas-keymap (kbd "C-k") 'yas-prev-field)
    (define-key yas-keymap (kbd "<escape>") 'yas-escape)
    (define-key yas-keymap (kbd "TAB") 'yas-next-field-or-maybe-expand)
    (define-key yas-keymap (kbd "TAB") nil)))

(defun yas-escape ()
  (interactive)
  (yas-abort-snippet)
  (evil-force-normal-state))

;;;----------------------------------------------------------------------------
;;; Company -- a good alternative of auto-complete

(use-package company
  :ensure t
  :defer t
  :init
  (progn
    (setq company-idle-delay 0.2
	  company-minimum-prefix-length 2
	  company-require-match nil
	  company-dabbrev-ignore-case nil
	  company-dabbrev-downcase nil)

    ;; disable company mode for below major modes.
    (setq company-global-modes '(not eshell-mode term-mode shell-mode))

    (add-hook 'after-init-hook 'global-company-mode))
  :config
  (progn
    (global-set-key "\t" 'tab-indent-or-complete))
  :diminish company-mode)

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

;;;----------------------------------------------------------------------------
;;; virtualenvwrapper
(use-package virtualenvwrapper
  :ensure t
  :init
  (progn
    (add-hook 'python-mode-hook (lambda ()
				  (venv-workon "localenv"))))
  :config
  (progn
    (venv-initialize-interactive-shells) ;; if you want interactive shell support
    (venv-initialize-eshell) ;; if you want eshell support
    (setq venv-location '("~/localenv"))))

;;;----------------------------------------------------------------------------
;;; auctex

(use-package tex
  :ensure auctex
  :config
  (progn
    (setq TeX-view-program-list
	  '(("SumatraPDF" "SumatraPDF.exe %o")
	    ("Gsview" "gsview32.exe %o")
	    ("Okular" "okular --unique %o")
	    ("Acroread" "acroread %o")
	    ("Evince" "evince %o")
	    ("Firefox" "firefox %o")
	    ("Zathura" "zathura %o")))
    (add-to-list 'TeX-command-list
		 '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil
		   (latex-mode doctex-mode)))
    (add-to-list 'TeX-command-list
		 '("XeTeX" "%`xetex%(mode)%' %t" TeX-run-TeX nil
		   (plain-tex-mode ams-tex-mode texinfo-mode)))
    (setq TeX-view-program-selection
	  '((output-pdf "Okular")
	    (output-dvi "Okular")))))

;;;-----------------------------------------------------------------------------
(use-package fill-column-indicator
  :ensure t
  :config
  (progn
    (setq-default fill-column 80)
    (setq fci-rule-character-color "red")))

;;;-----------------------------------------------------------------------------
;;; emmet-mode
(use-package emmet-mode
  :commands emmet-mode
  :ensure t
  :init
  (progn
    (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
    (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
    ))

;;;-----------------------------------------------------------------------------
;;; sk/fzf
(use-package sk
  :config
  (progn
    (define-key evil-normal-state-map (kbd "C-p") 'sk)
    (evil-leader/set-key "/" 'ag)))

;;;-----------------------------------------------------------------------------
;;; spaceline for modeline
(use-package spaceline-config
  :ensure spaceline
  :init
  (setq ns-use-srgb-colorspace nil)
  :config
  (progn
    (spaceline-spacemacs-theme)
    (setq spaceline-workspace-numbers-unicode t)
    (setq spaceline-window-numbers-unicode t)
    (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)))

;;;----------------------------------------------------------------------------
;;; window-numbering: jump to window with number
(use-package winum
  :ensure t
  :init
  (progn
    (setq winum-keymap nil))
  :config
  (progn
    ;; <leader> <num>
    (evil-leader/set-key
      "0" 'winum-select-window-0
      "1" 'winum-select-window-1
      "2" 'winum-select-window-2
      "3" 'winum-select-window-3
      "4" 'winum-select-window-4
      "5" 'winum-select-window-5
      "6" 'winum-select-window-6
      "7" 'winum-select-window-7
      "8" 'winum-select-window-8
      "9" 'winum-select-window-9)

    (setq winum-auto-setup-mode-line nil)
    (winum-mode)))

;;;============================================================================
;;; Filetype specified configuration

;;;-----------------------------------------------------------------------------
; c mode
(add-hook 'c-mode-common-hook
	  '(lambda ()
	     (c-set-style "K&R")
	     (setq c-basic-offset 4)))

;;;-----------------------------------------------------------------------------
;;; Python mode

(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;;;-----------------------------------------------------------------------------
;;; org mode

(use-package org
  :ensure org-plus-contrib
  :mode (("\\.org$" . org-mode))
  :config
  (progn
    (use-package evil-org :ensure t)
    (setq org-directory "~/Dropbox/wiki/org")))

(use-package org-bullets
  :ensure t
  :commands (org-bullets-mode)
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-journal
  :ensure t
  :bind (("C-c C-j" . org-journal-new-entry))
  :commands (org-journal-read-entry)
  :init
  (progn
    (custom-set-variables
     '(org-journal-dir "~/Dropbox/wiki/org/diary")
     '(org-journal-file-format "%Y%m%d.org")
     '(org-journal-hide-entries-p nil))))

;; (setq org-directory "~/org")

;; ;;; load org wiki settings (not loading this if on windows)
;; (if (not (eq system-type 'windows-nt))
;;     (require 'org-projects)
;;     )

;; (with-eval-after-load "org"
;;   ;;; evil-org-mode
;;   (require 'evil-org))

;; ; Enable literal links
;; (defun turn-on-literal-links ()
;;   "enable literal links."

;;   (org-remove-from-invisibility-spec '(org-link))
;;   (org-restart-font-lock))

;; (add-hook 'org-mode-hook 'turn-on-literal-links)

;; (define-key global-map "\C-cl" 'org-store-link)
;; (define-key global-map "\C-ca" 'org-agenda)
;; (define-key global-map "\C-cb" 'org-iswitchb)

;; ;;; settings for org mode capture
;; (setq org-default-notes-file (concat org-directory "/notes.org"))
;; (define-key global-map "\C-cc" 'org-capture)

;; ;;; test capture templates
;; (setq org-capture-templates
;;       '(("t" "Todo" entry (file+headline "~/org/newgtd.org" "Tasks")
;;          "* TODO %^{Brief Description} %^g\n%?\nAdded: %U")
;;         ("j" "Journal" entry (file+datetree "~/org/journal.org")
;; 	 "* %?\nEntered on %U\n  %i\n  %a")
;; 	("l" "Link" plain (file (concat org-directory "/links.org"))
;;          "- %?\n %x\n")
;; 	("n" "Note" entry (file (concat org-directory "/notes.org"))
;;          "**  %?\n%x\nAdded:%U\n")))

;; (setq org-refile-targets '(("newgtd.org" :maxlevel . 1)
;; 			   ("someday.org" :level . 1)
;;                            ("done.org" :level . 1)))

;; ;;; setup agenda
;; (setq org-agenda-files `(,(concat org-directory "/newgtd.org")))
;; (setq org-agenda-custom-commands
;;       '(("H" "Office and Home Lists"
;; 	 ((agenda)
;; 	  (tags-todo "OFFICE")
;; 	  (tags-todo "HOME")
;; 	  (tags-todo "COMPUTER")
;; 	  (tags-todo "DVD")
;; 	  (tags-todo "READING")))
;; 	("D" "Daily Action List"
;; 	 ((agenda "" ((org-agenda-ndays 1)
;; 		      (org-agenda-sorting-strategy
;; 		       '((agenda time-up priority-down tag-up)))
;; 		      (org-deadline-warning-days 0)))))))

;; ;;; babel settings
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((emacs-lisp . t)
;;    (sh . t)
;;    (python . t)
;;    (C . t)
;;    (awk . t)
;;    (ditaa . t)
;;    (gnuplot . t)
;;    (latex . t)
;;    (matlab . t)
;;    (scheme . t)))

;;;----------------------------------------------------------------------------
;;; eim -- Chinese wubi input method

(add-to-list 'load-path "~/.emacs.d/elisp/eim")

(use-package eim
  :commands eim-use-package

  :init
  ;; disable tooltip because it is not working
  (setq eim-use-tooltip nil)

  :config
  (add-hook 'eim-active-hook
	  '(lambda ()
	     (setq eim-page-length 7))))

(use-package eim-wb
  :defer t
  :config
  ;; 防止EIM直接上词
  (eim-set-option 'max-length 8))

(use-package eim-extra
  :defer t
  :bind ((";" . eim-insert-ascii-char)))

(register-input-method
 "eim-wb" "euc-cn" 'eim-use-package
 "五笔" "汉字五笔输入法" "wb.txt")
(register-input-method
 "eim-py" "euc-cn" 'eim-use-package
 "拼音" "汉字拼音输入法" "py.txt")
(register-input-method
 "eim-wbpy" "euc-cn" 'eim-use-package
 "五笔拼音" "汉字五笔拼音输入法" "wbpy.txt")

(setq default-input-method "eim-wb")

;;;============================================================================
;;; packages

;============================================================
; Additional Hacks
;============================================================

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; 1. Enable copy-paste with X clipboard (need xsel)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; http://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
;; I prefer using the "clipboard" selection (the one the
;; typically is used by c-c/c-v) before the primary selection
;; (that uses mouse-select/middle-button-click)
(setq x-select-enable-clipboard t)

;; If emacs is run in a terminal, the clipboard- functions have no
;; effect. Instead, we use of xsel, see
;; http://www.vergenet.net/~conrad/software/xsel/ -- "a command-line
;; program for getting and setting the contents of the X selection"
(unless window-system
 (when (getenv "DISPLAY")
  ;; Callback for when user cuts
  (defun xsel-cut-function (text &optional push)
    ;; Insert text to temp-buffer, and "send" content to xsel stdin
    (with-temp-buffer
      (insert text)
      ;; I prefer using the "clipboard" selection (the one the
      ;; typically is used by c-c/c-v) before the primary selection
      ;; (that uses mouse-select/middle-button-click)
      (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
  ;; Call back for when user pastes
  (defun xsel-paste-function()
    ;; Find out what is current selection by xsel. If it is different
    ;; from the top of the kill-ring (car kill-ring), then return
    ;; it. Else, nil is returned, so whatever is in the top of the
    ;; kill-ring will be used.
    (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
      (unless (string= (car kill-ring) xsel-output)
	xsel-output )))
  ;; Attach callbacks to hooks
  (setq interprogram-cut-function 'xsel-cut-function)
  (setq interprogram-paste-function 'xsel-paste-function)
  ;; Idea from
  ;; http://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
  ;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03577.html
 ))

;============================================================
; Settings by Emacs Groups
;============================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-toggle-key "C-`")
 '(fci-rule-color "#383838")
 '(inhibit-startup-screen t)
 '(ns-command-modifier (quote meta))
 '(org-journal-dir "~/Dropbox/wiki/org/diary")
 '(org-journal-file-format "%Y%m%d.org")
 '(org-journal-hide-entries-p nil)
 '(package-selected-packages
   (quote
    (fill-column-indicator yasnippet virtualenvwrapper use-package smex persp-mode neotree evil-visualstar evil-search-highlight-persist evil-paredit evil-numbers evil-nerd-commenter evil-matchit evil-leader company auctex ace-jump-mode)))
 '(persp-auto-resume-time 0)
 '(persp-auto-save-opt 0)
 '(persp-nil-name "main")
 '(scheme-program-name "racket"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-search-highlight-persist-highlight-face ((t (:background "red")))))
(put 'dired-find-alternate-file 'disabled nil)

;; (package-selected-packages
;;    (quote
;;     (use-package yasnippet virtualenvwrapper smex persp-mode neotree multi-term multi-eshell markdown-mode magit js2-mode irony idomenu htmlize helm expand-region evil-visualstar evil-surround evil-search-highlight-persist evil-paredit evil-org evil-numbers evil-nerd-commenter evil-matchit evil-leader evil-iedit-state emmet-mode emacs-eclim company cider ace-jump-mode)))
 ;; '(company-backends
 ;;   (quote
 ;;    (company-bbdb company-nxml company-css company-eclim company-semantic company-clang company-xcode company-ropemacs company-cmake company-capf
 ;; 		  (company-dabbrev-code company-gtags company-etags company-keywords)
 ;; 		  company-oddmuse company-files)))
