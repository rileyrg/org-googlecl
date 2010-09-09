(defcustom googlecl-blogname "My Blog Name"
  "The name of the default blogger/blogspot blog you wish to blog to."
  :group 'org-googlecl
  :type 'string)

(defcustom googlecl-username "changeme@googlemail.com"
  "The google user id you wish to authenticate with. e.g mydevusername@googlemail.com"
  :group 'org-googlecl
  :type 'string)

(defcustom googlecl-process-buffer "*googlecl*"
  "Name of the buffer the googlecl process logs to"
  :group 'org-googlecl
  :type 'string)

(defcustom googlecl-process-name "Google CLI"
  "Name of the process the gogole program uses"
  :group 'org-googlecl
  :type 'string)

(defun googlecl-blog (&optional borg btitle blabels bbody)

  (unless (setq org-googlecl-blogname (read-from-minibuffer "Blog Name:" googlecl-blogname)))
  (unless (setq btitle (read-from-minibuffer "Title:" btitle)))
  (unless (setq bbody (if (region-active-p) (region-or-word-at-point) (read-from-minibuffer "Body:" ))))
  (unless (setq blabels (read-from-minibuffer "Labels:" blabels)))

  (let*(
       (tmpfile (make-temp-file "blog-"))
       (tmpbuf (find-file-noselect tmpfile))
       (blog-command (concat "google blogger post --blog \"" googlecl-blogname "\"" (if (length btitle) (concat " --title \"" btitle "\"")) " --user \"" googlecl-username "\" " (if (length blabels) (concat " --tags \"" blabels "\" "))  tmpfile)))
   (if borg
	 (org-export-as-html 1 nil nil tmpbuf t))
   (with-current-buffer tmpbuf
       (if (not borg)
	   (insert (concat "" bbody "")))
       (save-buffer))
   (message "%s" blog-command)
   (start-process-shell-command googlecl-process-name googlecl-process-buffer blog-command)))
        

(defun org-googlecl-blog  ()
  "Post the current org item to blogger/blogspot. Tags are converted to blogger labels. If you wish to alter the default blog name prefix the function call (C-u)."
  (interactive)
  (if current-prefix-arg
      (setq googlecl-blogname (read-from-minibuffer "Blog Name:")))
  (save-excursion
    (goto-char (org-entry-beginning-position))
    (let ((blabels (mapconcat  'identity (org-get-tags) ","))
	  (btitle (org-trim (replace-regexp-in-string (org-get-tags-string) "" (org-get-heading)) )))
      (set-mark (org-entry-end-position))
      (googlecl-blog t btitle blabels))))

(provide 'org-googlecl)
