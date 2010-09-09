(defcustom org-googlecl-blogname "My Blog Name"
  "The name of the default blogger/blogspot blog you wish to blog to."
  :group 'org-googlecl
  :type 'string)

(defcustom org-googlecl-username "changeme@googlemail.com"
  "The google user id you wish to authenticate with. e.g mydevusername@googlemail.com"
  :group 'org-googlecl
  :type 'string)

  (defun org-googlecl-blog  ()
    (interactive)
    (if current-prefix-arg
        ; WOuld be nice to be able to query possible blogs and allow tab completion on legal names.
        (setq org-googlecl-blogname (read-from-minibuffer "Blog Name:")))
    (save-excursion
      (goto-char (org-entry-beginning-position))
      (let ((tmptags (mapconcat  'identity (org-get-tags) ","))
	    (tmpheading (org-trim (replace-regexp-in-string (org-get-tags-string) "" (org-get-heading)) )))
          (set-mark (org-entry-end-position))
          (let*((tmpfile (make-temp-file "org-blog-html-"))
               (blog-command (concat "google blogger post --blog \"" org-googlecl-blogname "\" --title \"" tmpheading "\" --user \"" org-googlecl-username (if (length tmptags) (concat "\" --tags \"" tmptags "\" "))  tmpfile )))
            (org-export-as-html 1 nil nil (find-file-noselect tmpfile) t)
            (with-current-buffer (get-file-buffer tmpfile) (save-buffer))
            (start-process-shell-command "Google Blog" "*googlecl*" blog-command)
            ))))

(provide 'org-googlecl)
