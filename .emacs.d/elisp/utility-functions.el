;;; region replace macro
(defmacro region-replace-routine (regex literal start end)
  `(let ((end (or ,end (and (use-region-p) (region-end)) (point-max)))
         (start (or ,start (and (use-region-p) (region-beginning)) (point-min))))
     (goto-char start)
     (while (re-search-forward ,regex end t)
       (replace-match ,literal nil nil))))

(defun region-replace (regex literal &optional start end)
  "Replace regex to literal in a region if start/end are not specified."
  (interactive)
  (save-excursion
    (barf-if-buffer-read-only)
    (region-replace-routine regex literal start end)))

(defun swaps (left right &optional start end)
  "Swaps left to right."
  (interactive "sChange this: 
sTo this: " )
  (save-excursion
    (barf-if-buffer-read-only)
    (let ((left (regexp-quote left))
          (right (regexp-quote right))
          (case-replace nil)            ; re-consider if this is needed.
          (case-fold-search nil))
      (region-replace-routine (format "\\\(%s\\\)\\|\\\(%s\\\)" left right)
                              (if (match-string 1) right left)
                              start
                              end))))

;;; Helper function for defining key bindings.
(defmacro define-key-with-fallback (keymap key def condition &optional mode)
  "Define key with fallback. Binds KEY to definition DEF in keymap KEYMAP, 
   the binding is active when the CONDITION is true. Otherwise turns MODE off 
   and re-enables previous definition for KEY. If MODE is nil, tries to recover 
   it by stripping off \"-map\" from KEYMAP name."
  `(define-key ,keymap ,key
     (lambda () (interactive)
       (if ,condition ,def
	 (let* ((,(if mode mode
		    (let* ((keymap-str (symbol-name keymap))
			   (mode-name-end (- (string-width keymap-str) 4)))
		      (if (string= "-map" (substring keymap-str mode-name-end))
			  (intern (substring keymap-str 0 mode-name-end))
			(error "Could not deduce mode name from keymap name (\"-map\" missing?)")))) 
		 nil)
		(original-func (key-binding ,key)))
	   (call-interactively original-func))))))

(provide 'utility-functions)
