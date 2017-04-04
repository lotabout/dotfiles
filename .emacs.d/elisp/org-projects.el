;;; settup org projects
(require 'org)
(require 'htmlize)
(require 'ox-publish)
(setq ;org-export-default-language "en"
      org-export-html-extension "html"
      org-export-with-timestamps t
      org-export-with-section-numbers nil
      org-export-with-tags 'not-in-toc
      org-export-skip-text-before-1st-heading nil
      org-export-with-sub-superscripts '{}
      org-export-with-LaTeX-fragments t
      org-export-with-archived-trees nil
      org-export-highlight-first-table-line t
      org-export-latex-listings-w-names nil
      org-html-head-include-default-style nil
      org-html-doctype "html5"
      org-html-head ""
      org-html-metadata-timestamp-format "%Y-%m-%d"
      org-export-htmlize-output-type 'css
      org-startup-folded nil
      org-export-allow-BIND t
      org-publish-list-skipped-files t
      org-publish-use-timestamps-flag t
      org-export-babel-evaluate nil
      org-confirm-babel-evaluate t)

(eval-after-load "ox-html"
  '(setq org-html-scripts
	 (concat org-html-scripts "\n"
		 "<script type=\"text/javascript\">
    function show_org_source(){
       /* document.location.href = rpl(document.location.href,\"html\",\"org.html\"); */
       var url = document.location.href.split('#')[0];
       document.location.href = url.replace(/.html$/, '.org.html');
    }
</script>")))

(setq wm-base "~/Dropbox/org/orgwiki/")
(setq wm-htmlroot "~/Dropbox/org/orgwiki_html/")
(setq wm-base-directory wm-base)
(setq wm-base-style-directory (concat wm-base "assets/css/"))
(setq wm-base-code-directory (concat wm-base "code/"))
(setq wm-base-color-themes-directory (concat wm-base "color-themes/"))
(setq wm-base-images-directory (concat wm-base "images/"))
(setq wm-publish-directory wm-htmlroot)
(setq wm-publish-style-directory (concat wm-htmlroot "assets/css/"))

(defun set-org-publish-project-alist ()
  "Set Publish projects"
  (interactive)
  (setq org-publish-project-alist
	`(("org-notes"
	   :base-directory ,wm-base-directory
	   :base-extension "org"
	   :exclude "FIXME"
	   :makeindex t
	   :auto-sitemap t
	   :sitemap-ignore-case t
	   :html-extension "html"
	   :publishing-directory ,wm-publish-directory
	   ;:publishing-function (org-publish-org-to-html org-publish-org-to-org)
	   :publishing-function (org-html-publish-to-html org-org-publish-to-org)
	   :htmlized-source t ; use google-prettify instead.
	   :section-numbers nil
	   :table-of-contents t
	   :html-head "<link rel=\"Stylesheet\" type=\"text/css\" href=\"assets/css/style.css\"/>"
	   :recursive t
	   :html-preamble ,(with-temp-buffer (insert-file-contents (concat wm-base-directory "preamble.html")) (buffer-string))
	   :html-postamble ,(with-temp-buffer (insert-file-contents (concat wm-base-directory "postamble.html")) (buffer-string)))
	  ("org-static"
	   :base-directory ,wm-base-directory
	   :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|el"
	   :recursive t
	   :publishing-directory ,wm-publish-directory
	   :publishing-function org-publish-attachment)
	  ("org" :components ("org-notes" "org-static")))))


(set-org-publish-project-alist)

(defun han/org-html-delete-extra-space (string backend info)
  "Let chinese word to a line if match /han\newline/"
  (when (and (org-export-derived-backend-p backend 'html)
             (string-match "\\cc\n" string))
    (replace-regexp-in-string "\\(\\cc\\)\n\\(\\cc\\)" "\\1\\2" string)))

(eval-after-load 'ox-html
  '(add-to-list 'org-export-filter-final-output-functions
                'han/org-html-delete-extra-space))

(provide 'org-projects)
