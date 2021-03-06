;==============================================================================
;;; General settings

;;; custom scripts
(add-to-list 'load-path "~/.emacs.d/elisp")

;; package archive
(when (>= emacs-major-version 24)
  (setq package-archives '(("org" . "http://orgmode.org/elpa/")
                           ("gnu"   . "http://elpa.emacs-china.org/gnu/")
                           ;("melpa" . "http://elpa.emacs-china.org/melpa/")
                           ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                           ;("melpa" . "https://melpa.org/packages/")
                           ))
  (package-initialize))
(defvar my-packages '(use-package org))

(defun my-remove-if-not (predicate sequence)
  (delq nil (mapcar (lambda (x) (and (not (funcall predicate x)) x)) sequence)))

;; load use-package for package management
(let* ((package--builtins '())
       (missing (my-remove-if-not #'package-installed-p my-packages)))
  (print missing)
  (when missing
    (package-refresh-contents)
    (mapc 'package-install missing)))

(eval-when-compile
  (require 'use-package))

(require 'diminish)
(require 'bind-key)

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
            ;;(set-face-background 'hl-line "gray28")
            ))

;;; Show matched parents
(show-paren-mode t)

;;; turn on auto-fill mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;; disable splash screen and minibuffer messages on startup
(setq inhibit-startup-screen 1)
(setq inhibit-splash-screen t)
(setq inhibit-startup-echo-area-message t)

(setq ring-bell-function 'ignore)

;;; backup directory
(setq backup-directory-alist `(("." . "~/.emacs-backup"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;;; Tab behavior
(setq-default indent-tabs-mode nil)
(electric-indent-mode 1)

;;; OS specific default.
(cond
 ((string-equal system-type "darwin")
  (progn
    ;; Use Meta as Alt in OSX
    (customize-set-variable 'ns-command-modifier 'meta)
    (set-default-font "Source Code Pro-14")
    ))
 ((string-equal system-type "gnu/linux")
  (progn
    (when window-system
      (progn
	;; English Font
	(set-frame-font "DejaVu Sans Mono-11")
	;; Chinese Font
	(dolist (charset '(kana han symbol cjk-misc bopomofo))
	  (set-fontset-font (frame-parameter nil 'font)
			    charset
			    (font-spec :family "WenQuanYi Micro Hei Mono")))
	(setq face-font-rescale-alist '(("WenQuanYi Micro Hei Mono" . 1.2))))))))


;; set up PATH
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; turn on save place so that when opening a file, the cursor will be at the last position.
(if (>= emacs-major-version 25)
    (save-place-mode 1)
  (progn
      (require 'saveplace)
      (setq-default save-place t)))

;;; scroll by one line.
(setq scroll-conservatively 1)
(setq scroll-step 1)

;;; disable message of ad-redefinition
(setq ad-redefinition-action 'accept)

;;; Utilities
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    (declare (indent 1) (debug t))
    `(eval-after-load ,file '(progn ,@body))))

;; use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

(require 'utility-functions)

;; color theme
(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

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
      :config
      (progn
        (global-evil-leader-mode)
	(evil-leader/set-leader "SPC")

	(evil-leader/set-key
	  "q" 'kill-buffer
	  "w" 'save-buffer
	  "f" 'find-file
          "b" 'switch-to-buffer

	  ;; remove Ctrl-M
	  "m" '(lambda () (interactive) (region-replace "" ""))

	  ;; collapse blank lines
	  "<RET>" '(lambda () (interactive) (region-replace "^\n\\{2,\\}" "\n"))

          " " 'delete-trailing-whitespace))))

  :config
  (progn
    ;; enable evil by default
    (evil-mode 1)
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

    ;; clear screen and highlight
    (define-key evil-normal-state-map (kbd "C-d") '(lambda ()
                                                     (interactive)
                                                     (evil-search-highlight-persist-remove-all)
                                                     (redraw-frame)))

    (define-key evil-normal-state-map (kbd "SPC TAB") 'evil-switch-to-windows-last-buffer)

    ;; disable evil in these modes
    (setq evil-emacs-state-modes
          (append evil-emacs-state-modes
		  '(cider-repl-mode
		    cider-stacktrace-mode
		    eclim-project-mode))))
  :diminish undo-tree-mode)

;;; helper functions

(defun evil-paste-select ()
  "Visual select the pasted content."
  (interactive)
  (let ((begin (nth 3 evil-last-paste))
	(end (- (nth 4 evil-last-paste) 1)))
    (evil-visual-select begin end)))

(use-package evil-evilified-state)

(use-package evil-visualstar
  :ensure t
  :defer t)

(use-package evil-nerd-commenter
  :ensure t
  :commands evilnc-comment-or-uncomment-lines
  :init
  (evil-leader/set-key "cc" 'evilnc-comment-or-uncomment-lines))

(use-package evil-numbers
  :ensure t
  :defer t
  :init
  (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt))


(use-package evil-matchit
  :ensure t
  :commands global-evil-matchit-mode
  :init
  (global-evil-matchit-mode 1))

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
    (add-hook 'clojure-mode-hook 'evil-paredit-mode)
    (add-hook 'racket-mode-hook 'paredit-mode)
    (add-hook 'racket-mode-hook 'evil-paredit-mode)))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)))

(use-package evil-smartparens
  :ensure t
  :commands evil-smartparens-mode
  :diminish evil-smartparens-mode
  :defer t
  :init
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

(use-package evil-search-highlight-persist
  :ensure t
  :defer t
  :config
  (custom-set-faces
     `(evil-search-highlight-persist-highlight-face ((t (:inherit isearch))))))

(use-package evil-matchit
  :ensure t
  :defer t)

;;;----------------------------------------------------------------------------
;;; ace-jump -- i.e easymotion for vim
(use-package ace-jump-mode
  :ensure t
  :commands ac-jump-mode
  :init
  (define-key evil-normal-state-map (kbd "f") 'ace-jump-char-mode)
  (evil-leader/set-key "l" 'ace-jump-line-mode))

;;;----------------------------------------------------------------------------
;;; smex
;; (use-package smex
;;   :ensure t
;;   :config (smex-initialize)
;;   :bind (("M-x" . smex)
;; 	 ("M-X" . smex-major-mode-commands)) )

;;;----------------------------------------------------------------------------
;;; ido mode
;; (use-package ido
;;   :config
;;   (setq ido-enable-flex-matching t)
;;   (ido-everywhere t)
;;   (ido-mode 1))

;;;----------------------------------------------------------------------------
;; save undo history
(use-package undohist
  :ensure t
  :init
  (customize-set-variable 'undohist-directory "~/.emacs-undo")
  :config
  (undohist-initialize))

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
    (define-key persp-key-map (kbd "n") #'persp-next)
    (define-key persp-key-map (kbd "p") #'persp-prev)
    (define-key persp-key-map (kbd "r") #'persp-rename)
    (define-key persp-key-map (kbd "k") #'persp-kill)
    (define-key persp-key-map (kbd "a") #'persp-add-buffer)
    (define-key persp-key-map (kbd "t") #'persp-temporarily-display-buffer)
    (define-key persp-key-map (kbd "i") #'persp-import-buffers)
    (define-key persp-key-map (kbd "q") #'persp-remove-buffer)
    (define-key persp-key-map (kbd "w") #'persp-save-state-to-file)
    (define-key persp-key-map (kbd "l") #'persp-load-state-from-file)
    (define-key persp-key-map (kbd "SPC") #'zoom-window-zoom)))

;;;----------------------------------------------------------------------------
;;; zoom current window temporarily
(use-package zoom-window
  :ensure t
  :commands zoom-window-zoom
  :init
  (evil-leader/set-key "z" 'zoom-window-zoom)
  :config
  (progn
    (custom-set-variables '(zoom-window-use-persp t)
			  '(zoom-window-mode-line-color "cyan4"))))

;;;----------------------------------------------------------------------------
;;; projectile
(use-package projectile
  :commands (projectile-ack
             projectile-ag
             projectile-compile-project
             projectile-dired
             projectile-find-dir
             projectile-find-file
             projectile-find-tag
             projectile-test-project
             projectile-grep
             projectile-invalidate-cache
             projectile-kill-buffers
             projectile-multi-occur
             projectile-project-p
             projectile-project-root
             projectile-recentf
             projectile-regenerate-tags
             projectile-replace
             projectile-replace-regexp
             projectile-run-async-shell-command-in-root
             projectile-run-shell-command-in-root
             projectile-switch-project
             projectile-switch-to-buffer
             projectile-vc)
  :init
  (customize-set-variable 'projectile-keymap-prefix "")
  :config
  (projectile-global-mode t)
  :diminish projectile-mode)

;;;----------------------------------------------------------------------------
(use-package ranger
  :ensure t
  :commands (ranger)
  :bind ("<f8>" . ranger)
  :init
  (progn
    (setq ranger-max-preview-size 10)
    (setq ranger-dont-show-binary t)))

;;;----------------------------------------------------------------------------
;;; file tree, replace neotree.
(use-package direx
  :ensure t
  :commands (direx:jump-to-directory-other-window)
  :init
  (progn
    (evil-leader/set-key
      "ne" 'direx:jump-to-directory-other-window))
  :config
  (progn
    ;; integrate with popwin so that it serves as sidebar
    (when (boundp 'popwin:special-display-config)
      (push '(direx:direx-mode :position left :width 25 :dedicated t)
            popwin:special-display-config))

    ;; disable per-item keymap
    (setq direx:file-keymap nil)

    (evil-define-key 'normal direx:direx-mode-map
      (kbd "TAB") 'direx:toggle-item
      (kbd "q") 'quit-window
      (kbd "RET") 'direx:maybe-find-item
      (kbd "o") 'direx:maybe-find-item
      (kbd "j") 'direx:next-item
      (kbd "k") 'direx:previous-item
      (kbd "J") 'direx:next-sibling-item
      (kbd "K") 'direx:previous-sibling-item
      (kbd "e") 'direx:echo-item
      (kbd "f") 'direx:find-item
      (kbd "v") 'direx:view-item
      (kbd "V") 'direx:view-item-other-window
      (kbd "O") 'direx:expand-item-recursively
      (kbd "r") 'direx:refresh-whole-tree
      (kbd "c") 'direx:create-directory
      (kbd "R") 'direx:do-rename-file
      (kbd "C") 'direx:do-copy-files
      (kbd "D") 'direx:do-delete-files
      (kbd "M") 'direx:do-chmod-file
      (kbd "L") 'direx:do-load-file
      (kbd "B") 'direx:do-byte-compile-file
      (kbd "G") 'direx:do-chgrp
      (kbd "T") 'direx:do-touch)))

;;;----------------------------------------------------------------------------
;;; NeoTree -- NERD-tree for emacs
;; (use-package neotree
;;   :ensure t
;;   :commands (neotree-toggle neotree-find)
;;   :init
;;   (evil-leader/set-key
;;     "ne" 'neotree-toggle
;;     "nf" 'neotree-find)
;;   :config
;;   (progn
;;     (setq neo-theme 'ascii)

;;     (evil-define-key 'normal neotree-mode-map
;;       (kbd "TAB") 'neotree-enter
;;       (kbd "SPC") 'neotree-enter
;;       (kbd "q") 'neotree-hide
;;       (kbd "RET") 'neotree-enter
;;       (kbd "o") 'neotree-enter
;;       (kbd "j") 'neotree-next-line
;;       (kbd "k") 'neotree-previous-line
;;       (kbd "M") 'neotree-create-node
;;       (kbd "R") 'neotree-rename-node
;;       (kbd "D") 'neotree-delete-node
;;       (kbd "r") 'neotree-refresh
;;       (kbd "C") 'neotree-change-root
;;       (kbd "U") 'neotree-change-root
;;       (kbd "O") 'neotree-open-directory-recursively
;;       (kbd "H") 'neotree-hidden-file-toggle)))

;; (defun neotree-expand-node-descendants (&optional arg)
;;   "Expand the line under the cursor and all descendants.
;; Optional argument ARG indicates that any cache should be flushed."
;;   (interactive "P")
;;   (let ((full-path (if arg arg (neo-buffer--get-filename-current-line))))
;;     (when (file-directory-p full-path)
;;       (neo-buffer--set-expand full-path t)
;;       ; recursive expand the nodes;
;;       (dolist (node (car (neo-buffer--get-nodes full-path)))
;;         (neotree-expand-node-descendants node)))))

;; (defun neotree-open-directory-recursively (&optional arg)
;;   "Expand a directory recursively."
;;   (interactive "P")
;;   (neotree-expand-node-descendants arg)
;;   (neo-buffer--refresh t))

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
    (use-package init-eshell)
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
      (progn (switch-to-buffer b)
             (evil-insert-state)))))

;;;----------------------------------------------------------------------------
;;; multi-term
(use-package multi-term
  :ensure t
  :bind ("\C-ct" . get-term)
  :init
  (progn
    (setq multi-term-program "/bin/zsh")
    (setq term-unbind-key-list '("C-x" "C-c" "C-h" "M-x" "M-:")))
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
  (let ((b (last-term-buffer
	    (if (fboundp 'with-persp-buffer-list)
		(with-persp-buffer-list () (buffer-list))
	      (buffer-list)))))
    (if (or (not b) (eq 'term-mode major-mode))
	(progn (multi-term)
	       (when (fboundp 'persp-add-buffer)
		 (persp-add-buffer (current-buffer))))
      (switch-to-buffer b))))

;; Terminal buffer configuration.
(when (= emacs-major-version 24)
  (defun my-term-mode-hook ()
    ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=20611
    (setq bidi-paragraph-direction 'left-to-right))
  (add-hook 'term-mode-hook 'my-term-mode-hook))

;;;----------------------------------------------------------------------------
;;; which key
(use-package which-key
  :ensure t
  :diminish which-key-mode
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
  "Return to evil normal mode when escape from yas mode."
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
  (let ((yas-maybe-expand 'return-nil))
    (yas-expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas-minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

;;;----------------------------------------------------------------------------
;;; virtualenvwrapper
(use-package virtualenvwrapper
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'python-mode-hook (lambda ()
				  (venv-workon "localenv-py3"))))
  :config
  (progn
    (venv-initialize-interactive-shells) ;; if you want interactive shell support
    (venv-initialize-eshell) ;; if you want eshell support
    (setq venv-location '("~/localenv-py3"))))

;;;----------------------------------------------------------------------------
;;; auctex

(use-package tex
  :ensure auctex
  :defer t
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
  :commands (sk sk-directory sk/run)
  :init
  (progn
    (define-key evil-normal-state-map (kbd "C-p") 'sk)

    ;; use C-p in eshell
    (evil-define-key 'insert eshell-mode-map
      (kbd "C-p") 'sk)))

(use-package sk-extra
  :commands (ag ag-directory)
  :init
  (progn
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
    (setq winum-keymap nil)
    ;; <leader> <num>
    )
  :config
  (progn
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

;;;-----------------------------------------------------------------------------
;;; Magit
(use-package magit
  :ensure t
  :defer t
  :config
  (progn
    (use-package evil-magit
      :ensure t)))

;;;-----------------------------------------------------------------------------
(use-package ivy
  :ensure t
  :defer t
  :diminish ivy-mode
  :config
  (ivy-mode 1))

(use-package swiper
  :ensure t
  :bind ("C-s" . swiper))

(use-package counsel
  :ensure t
  :bind ("M-x" . counsel-M-x))

;;;-----------------------------------------------------------------------------
;;; hydrda, Free from typing the same prefixk eys
(use-package hydra
  :ensure t)

;;;-----------------------------------------------------------------------------
;;; flycheck
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :commands (flycheck-mode)
  :init
  (progn
    (add-hook 'prog-mode-hook 'flycheck-mode))
  :config
  (progn
    (custom-set-variables
     '(flycheck-check-syntax-automatically (quote (save mode-enabled))))
    (defhydra hydra-flycheck-error (flycheck-mode-map "C-c !")
      "navigate through errors"
      ("n" flycheck-next-error)
      ("p" flycheck-previous-error))
    ))

;;;-----------------------------------------------------------------------------
;;; popwin, manages special buffers as pop up windows
(use-package popwin
  :ensure popwin
  :config
  (progn
    (push '("*ag*" :noselect t) popwin:special-display-config)
    (push '("*Warnings*" :noselect t) popwin:special-display-config)
    (push '("*Help*") popwin:special-display-config)

    (popwin-mode 1)))

;;;-----------------------------------------------------------------------------
(use-package tramp
  :defer t
  :init
  ;; somehow scp and ssh won't work
  (setq tramp-default-method "sshx"))

;;;============================================================================
;;; Filetype specified configuration

;;;-----------------------------------------------------------------------------
;;; org mode

(use-package org
  ;; :ensure org-plus-contrib
  :ensure t
  :defer t
  :mode (("\\.org$" . org-mode))
  :init
  (progn
    ;; completion
    (defun my-org-mode-hook ()
      (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
    (add-hook 'org-mode-hook #'my-org-mode-hook)

    (global-set-key "\C-cl" 'org-store-link)
    (global-set-key "\C-ca" 'org-agenda)
    (global-set-key "\C-cc" 'org-capture)
    (global-set-key "\C-cb" 'org-iswitchb)

    )
  :config
  (progn
    (use-package evil-org
      :ensure t
      :config
      (progn
        (evil-define-key 'normal evil-org-mode-map
          "O" 'evil-open-above
          "J" 'evil-join
          "H" 'evil-window-top
          ;; for terminal
          "\t" 'org-cycle)))
    (setq org-directory "~/Dropbox/wiki/org")
    (setq org-export-coding-system 'utf-8)

    (setq org-use-fast-todo-selection t)
    (setq org-todo-keywords
          '((sequence "TODO(t)" "DOING(i)" "|" "DONE(d)")
            (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING")))

    (setq org-todo-keyword-faces
          '(("TODO" :foreground "red" :weight bold)
            ("DOING" :foreground "blue" :weight bold)
            ("DONE" :foreground "forest green" :weight bold)
            ("WAITING" :foreground "orange" :weight bold)
            ("HOLD" :foreground "magenta" :weight bold)
            ("CANCELLED" :foreground "forest green" :weight bold)
            ("MEETING" :foreground "forest green" :weight bold)
            ("PHONE" :foreground "forest green" :weight bold)))

    ;; TODO state triggers
    (setq org-todo-state-tags-triggers
          '(("CANCELLED" ("CANCELLED" . t))
            ("WAITING" ("WAITING" . t))
            ("HOLD" ("WAITING") ("HOLD" . t))
            (done ("WAITING") ("HOLD"))
            ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
            ("DOING" ("WAITING") ("CANCELLED") ("HOLD"))
            ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))

    ;; Add timestamp then the status is changed to "DONE"
    (setq org-log-done 'time)

    ;; settings for org mode capture
    (setq org-default-notes-file (concat org-directory "/journal.org"))

    ;; test capture templates
    (setq org-capture-templates
	  '(("j" "Journal" entry (file+datetree (concat org-directory "/journal.org"))
	     "* TODO %<%H:%M> %?\nSCHEDULED: %t  %i\n  %a" :clock-in t :clock-resume t)
	    ("l" "Link" plain (file (concat org-directory "/links.org"))
	     "- %?\n\n"
             :clock-in t :clock-resume t)
	    ("n" "Note" entry (file+headline (concat org-directory "/journal.org") "Unhandled")
	     "* %?\n\n"
             :clock-in t :clock-resume t)
	    ("i" "Idea" entry (file+headline (concat org-directory "/journal.org") "Ideas")
	     "* %?\n\n"
             :clock-in t :clock-resume t)
	    ("c" "Code Snippets" entry (file+headline (concat org-directory "/journal.org") "Code Snippets")
	     "* %\n  #+begin_src %^{Language}\n%x#+end_src\n"
             :clock-in t :clock-resume t)))

    (setq org-refile-targets '(("journal.org" :maxlevel . 3)))

    ;; babel settings
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (sh . t)
       (python . t)
       (C . t)
       (awk . t)
       (ditaa . t)
       (gnuplot . t)
       (latex . t)
       (scheme . t)
       (racket . t)))
    ))

(use-package org-agenda
  :defer t
  :config
  (progn
    (setq org-agenda-restore-windows-after-quit t)

    ;; setup agenda
    (setq org-agenda-files `(,(concat org-directory "/journal.org")))
    (setq org-agenda-custom-commands
	  '(("D" "Daily Action List"
	     ((agenda "" ((org-agenda-ndays 1)
			  (org-agenda-sorting-strategy
			   '((agenda time-up priority-down tag-up)))
			  (org-deadline-warning-days 0)))))))

    ;; evilify org-agenda-mode
    (evilified-state-evilify-map org-agenda-mode-map
      :mode org-agenda-mode
      :bindings
      "j" 'org-agenda-next-line
      "k" 'org-agenda-previous-line
      "c" 'org-agenda-columns
      (kbd "M-j") 'org-agenda-next-item
      (kbd "M-k") 'org-agenda-previous-item
      (kbd "M-h") 'org-agenda-earlier
      (kbd "M-l") 'org-agenda-later
      (kbd "gd") 'org-agenda-toggle-time-grid
      (kbd "gr") 'org-agenda-redo
      (kbd "M-RET") 'org-agenda-show-and-scroll-up)
    ))

(use-package org-bullets
  :ensure t
  :commands (org-bullets-mode)
  :defer t
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(defun notify-linux (title message &optional timeout)
  (call-process "notify-send"
                nil 0 nil
                "-t" (if timeout (number-to-string timeout) "3000")
                title
                message
                ))

(use-package org-pomodoro
  :ensure t
  :commands (org-pomodoro)
  :defer t
  :config
  (setq org-pomodoro-long-break-length 10)
  (add-hook 'org-pomodoro-finished-hook
            (lambda ()
              (notify-linux "Pomodoro completed!" "Time for a break." (* 5 60 1000))))
  (add-hook 'org-pomodoro-break-finished-hook
            (lambda ()
              (notify-linux "Pomodoro Short Break Finished" "Ready for Another?")))
  (add-hook 'org-pomodoro-long-break-finished-hook
            (lambda ()
              (notify-linux "Pomodoro Long Break Finished" "Ready for Another?")))
  (add-hook 'org-pomodoro-killed-hook
            (lambda ()
              (notify-linux "Pomodoro Killed" "One does not simply kill a pomodoro!"))))

;;;----------------------------------------------------------------------------
;;; markdown mode
(use-package markdown-mode
  :ensure t
  :mode (("\\.md$" . markdown-mode)))

;;;-----------------------------------------------------------------------------
; c mode
(add-hook 'c-mode-common-hook
	  '(lambda ()
	     (c-set-style "K&R")
	     (setq c-basic-offset 4)))

;;;-----------------------------------------------------------------------------
;;; Python mode

(use-package python
  :ensure t
  :mode (("\\.py" . python-mode))
  :init
  :config
  (progn
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "--simple-prompt -i"
          python-shell-prompt-regexp "In \\[[0-9]+\\]: "
          python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
          python-shell-completion-setup-code
            "from IPython.core.completerlib import module_completion"
          python-shell-completion-string-code
          "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

    (push '("*Python*" :noselect t) popwin:special-display-config)

    ;; change the key binding for eval in region
    (evil-define-key 'visual python-mode-map
      "\C-c\C-c" 'python-shell-send-region)
    (evil-leader/set-key-for-mode 'python-mode
      "'" 'python-shell-switch-to-shell)

    (add-hook 'python-mode-hook
              '(lambda ()
                 (local-set-key (kbd "<C-return>") 'python-shell-send-defun)))))

;;; completion
(use-package company-jedi
  :ensure t
  :mode (("\\.py" . python-mode))
  :init
  (progn
    (defun my/python-mode-hook ()
      (add-to-list 'company-backends 'company-jedi))
    (add-hook 'python-mode-hook 'my/python-mode-hook)))

;;;-----------------------------------------------------------------------------
;; hy mode
(use-package hy-mode
  :ensure t
  :mode (("\\.hy" . hy-mode)))

;;;-----------------------------------------------------------------------------
;;; clojure
(use-package clojure-mode
  :ensure t
  :mode (("\\.clj" . clojure-mode)
         ("\\.cljs" . clojurescript-mode))
  :config
  (progn
    (evil-leader/set-key-for-mode 'clojure-mode
      "'" 'cider-switch-to-repl-buffer)
    (evil-leader/set-key-for-mode 'clojurescript-mode
      "'" 'cider-switch-to-repl-buffer)
    (push '("\*.*cider-repl.*\*" :regexp t :noselect t) popwin:special-display-config)))

;;; completion
(use-package cider
  :ensure t
  :commands cider-jack-in)

;;;-----------------------------------------------------------------------------
;; racket mode
(use-package racket-mode
  :ensure t
  :mode (("\\.rkt" . racket-mode)
         ("\\.rktl" . racket-mode))
  :config
  (progn
    (push '("*Racket REPL*" :noselect t) popwin:special-display-config)
    (evil-leader/set-key-for-mode 'racket-mode
      "'" 'racket-repl)

    ;; overwrite the get-type method, I think it is too slow, and useless.
    (defun racket--get-type (str))

    (define-key racket-mode-map (kbd "<C-return>") 'racket-send-definition)
    (define-key racket-mode-map (kbd "C-x C-e") 'racket-send-last-sexp)))

;;;============================================================================
;;; Additional packages

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

;;;----------------------------------------------------------------------------
;; Calendar mode
(use-package calendar
  :commands calendar-mode
  :config
  (progn
    (evilified-state-evilify-map calendar-mode-map
      :mode calendar-mode)))

;;;----------------------------------------------------------------------------
(use-package yaml-mode
  :mode (("\\.yaml" . yaml-mode)))

;;;============================================================================
; Additional Hacks

;;-----------------------------------------------------------------------------
;; copy file path
(defun clip-file-path-from-project-root ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      (file-name-directory default-directory)
                    (buffer-file-name)))
        (root (condition-case nil
                  (projectile-project-root)
                (error ""))))
    (when filename
      (kill-new (replace-regexp-in-string (concat "^" (regexp-quote root)) "" filename)))))

(defun clip-file-path-full ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      (file-name-directory default-directory)
                    (buffer-file-name))))
    (when filename
      (kill-new filename))))

;;;----------------------------------------------------------------------------
;;; Enable copy-paste with X clipboard (need xsel)

;; http://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
;; I prefer using the "clipboard" selection (the one the
;; typically is used by c-c/c-v) before the primary selection
;; (that uses mouse-select/middle-button-click)
(setq select-enable-clipboard t)

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

;;;============================================================================
;; variables for customize-groups (auto saved)
