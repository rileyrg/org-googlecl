(defcustom googlecl-blogname "My Blog Name"
  "The name of the default blogger/blogspot blog you wish to blog to."
  :group 'org-googlecl
  :type 'string)

(defcustom googlecl-username "changeme@googlemail.com"
  "The google user id you wish to authenticate with. e.g mydevusername@googlemail.com"
  :group 'org-googlecl
  :type 'string)

(defcustom googlecl-default-labels "emacs,elisp"
  "default labels"
  :group 'org-googlecl
  :type 'string)

(defcustom googlecl-blog-exists nil
  "Set to t if you wish to be informed if the item is already blogged. If set and the items exists you be prompted to post again or to browse the existing blog entry."
  :group 'org-googlecl
  :type 'boolean)

(defcustom googlecl-footer "<br/>-- <br/><a href='http://github.com/rileyrg'>My Emacs Files At GitHub</a>"
  "footer for the post"
  :group 'org-googlecl
  :type 'string)

(defcustom googlecl-prompt-footer nil
  "whether to prompt with the footer"
  :group 'org-googlecl
  :type 'boolean)

(defcustom googlecl-blog-exists nil
  "Set to t if you wish to be informed if the item is already blogged. If set and the items exists you be prompted to post again or to browse the existing blog entry."
  :group 'org-googlecl
  :type 'boolean)

(defcustom googlecl-blog-tag "gblog"
  "org entries that are blogged are tagged with this tag. Leave empty if you dont want your org entry tagged."
  :group 'org-googlecl
  :type 'string)

(defcustom googlecl-repost-tag "repost"
  "tag/label value for blog entries which have been reposted"
  :group 'org-googlecl
  :type 'string)

(defcustom googlecl-blog-auto-del nil
  "If set to t then will auto delete any existing blog entries with the same title"
  :group 'org-googlecl
  :type 'boolean)

(defcustom googlecl-blog-exists t
  "Set to t if you wish to be informed if the item is already blogged."
  :group 'org-googlecl
  :type 'boolean)

(defcustom googlecl-default-title-filter ".*"
  "regexp for searching blogger"
  :group 'org-googlecl
  :type 'string)

(defcustom googlecl-no-labels "N/A"
  "the repsonse from googlecl if the blog has no labels"
  :group 'org-googlecl
  :type 'string)

(defcustom googlecl-list-regexp "\\(.*\\),\\(http.*\\)\\(?:,\\(.*\\)\\)?$"
  "the regexp used to match the output from the google blogger list command"
  :group 'org-googlecl
  :type 'string)

(defun googlecl-prompt-blog ()
  "If in an org buffer prompt whether to blog the entire entry or to perform a normal text blog."
  (interactive)
  (if (eq major-mode 'org-mode)
      (if (yes-or-no-p "Blog the Org Entry?")
          (org-googlecl-blog) 
        (googlecl-blog))
    (googlecl-blog)))

(defun googlecl-blog (&optional borg btitle blabels bbody)
  "Generalised googlecl blog. Only prompt for blog,title and
labels if an org item as the rest comes from the org item at
point. If you wish to blog current org item set borg parameter to
t"
  (setq googlecl-blogname (read-from-minibuffer "Blog Name:" googlecl-blogname))
  (setq btitle (read-from-minibuffer "Title:" btitle))
  (unless borg 
    (setq bbody 
          (if (use-region-p) 
              (region-or-word-at-point) 
            (read-from-minibuffer "Body:" ))))
  (setq googlecl-default-labels 
        (setq blabels 
              (read-from-minibuffer 
               "Labels:" 
               (if (zerop (length blabels))
                   googlecl-default-labels
                 blabels))))
  (when googlecl-prompt-footer 
    (setq googlecl-footer (read-from-minibuffer "Footer:" googlecl-footer)))
  ;; If the option flag googlecl-blog-exists is set to true we check if there is already an entry with this title.
  ;; If a blog with the same title exists then give the option to view it via the default browser.
  (let (reposted)
    (when googlecl-blog-exists
      (with-temp-buffer
        (let* ((blogrc (call-process-shell-command  (format "google blogger  list --blog '%s' --title '%s' url" googlecl-blogname btitle) nil (current-buffer)))
               (blogurl (buffer-string)))
          (when (plusp (length blogurl))
            (when (and (not googlecl-blog-auto-del) 
                       googlecl-blog-exists
                       (y-or-n-p (concat "Blog entry exists :" blogurl ". View existing?")))
              (browse-url (nth 0 (org-split-string blogurl))))
            (setq blogurl (nth 0 (org-split-string blogurl)))
            (when (setq reposted (or googlecl-blog-auto-del (y-or-n-p "Delete existing blog entry?")))
              (googlecl-delete-blog btitle))))))
    (let* ((tmpfile (make-temp-file "googlecl"))
           (bhtml (if borg (org-export-as-html 5 nil nil 'string t) bbody))
           (blog-command (concat 
                          "google blogger post --blog \"" googlecl-blogname "\""
                          (if (length btitle) (concat " --title \"" btitle "\"")) " --user \"" googlecl-username "\" " 
                          (if (length blabels) (concat " --tags \"" (concat blabels (if (and reposted (plusp (length googlecl-repost-tag))) (concat "," googlecl-repost-tag) "")) "\" "))  
                          tmpfile)))
      (message "blog command is : %s" blog-command)
      (with-temp-file tmpfile
        (insert bhtml)
        (goto-char (buffer-end 1))
        (when googlecl-footer
          (insert googlecl-footer)))
      (start-process-shell-command "googlecl-pid" nil blog-command))))

(defun org-googlecl-blog  ()
  "Post the current org item to blogger/blogspot.
   Tags are converted to blogger labels. If you wish to alter the default blog name prefix the function call (C-u)."
  (interactive)
  (when current-prefix-arg
    (setq googlecl-blogname (read-from-minibuffer "Blog Name:")))
  (save-excursion
    (set-mark (goto-char (org-entry-beginning-position)))
    (let ((btags (org-get-tags)))
      (save-excursion
        (set-mark (goto-char (org-entry-beginning-position)))
        (let ((btitle (nth 4 (org-heading-components))))
          (org-forward-same-level 1 t)
          (googlecl-blog t btitle (mapconcat  'identity  (remove googlecl-blog-tag btags) ","))))
      (when (plusp (length googlecl-blog-tag))
        (org-set-tags-to (add-to-list 'btags googlecl-blog-tag))))))

(defun googlecl-delete-blog (btitle &optional filter)
  "delete blog(s) from specific blog with matching title. All such blog entries are removed."
  (let ((delcommand  (format "yes | google blogger delete --blog '%s' --title '%s'"  googlecl-blogname btitle)))
    (message "Delete command is : %s" delcommand)
    (set-process-filter (start-process-shell-command "google" nil delcommand)
                        filter)))

(defun googlecl-list-process (proc string)
  (with-current-buffer (process-buffer proc)
    (toggle-read-only 0)
    (delete-region (point-min) (point-max))
    (org-mode)
    (org-insert-heading)
    (insert (format " List of blogs with <%s> in the title\n\n" googlecl-default-title-filter))
    (let ((string (replace-regexp-in-string "\n$" "" string)))
      (message (format "length is %s" (length string)))
      (save-excursion
        (let ((first t))
          (dolist (item (split-string string "\n"))
            (when (string-match googlecl-list-regexp item)
              (let ((title (match-string 1 item))
                    (url (match-string 2 item))
                    (tags (match-string 3 item)))
                (save-match-data (org-insert-heading nil t))
                (insert (format "%s\n  %s" title url))
                (when (and tags (split-string tags ";"))
                  (org-set-tags-to (remove googlecl-no-labels (add-to-list 'taglist googlecl-blog-tag))))
                (when first
                  (setq first nil)
                  (org-back-to-heading)
                  (org-metaright)
                  (org-end-of-subtree)))))))))
  (switch-to-buffer (process-buffer proc))
  (let ((keymap (make-keymap)))
    (define-key keymap "d" (lambda () 
                             (interactive)
                             (org-googlecl-delete-entry (org-get-heading))))
    (define-key keymap "g" (lambda () 
                             (interactive)
                             (org-googlecl-list-blogs googlecl-blogname
                                                      googlecl-default-title-filter)))
    (define-key keymap "q" 'quit-window)
    (use-local-map keymap))
  (toggle-read-only 1))

(defun org-googlecl-delete-entry (title)
  (interactive "sTitle to delete: ")
  (when (y-or-n-p (format "Do you really delete entry '%s' ?" title))
    (googlecl-delete-blog title 
                          (lambda (proc string)
                            (org-googlecl-list-blogs googlecl-blogname
                                                     googlecl-default-title-filter)))))

(defun org-googlecl-list-blogs-aux (blogname title-filter)
  (let ((listblogcmd (format "google blogger list title,url,tags --title \"%s\" --blog \"%s\"" 
                             title-filter
                             blogname)))
    (message "List blog command is : %s" listblogcmd)
    (set-process-filter (start-process-shell-command "googlecl-list" "*googlcl blogs*" listblogcmd)
                        'googlecl-list-process)))

(defun org-googlecl-list-blogs (blogname title-filter)
  "accept a  title filter value and then list all blogs which match that value"
  (interactive 
   (list (read-from-minibuffer "Blog Name: " googlecl-blogname)
         (read-from-minibuffer "Title Contains: " googlecl-default-title-filter)))
  (setq googlecl-blogname blogname)
  (setq googlecl-default-title-filter title-filter) 
  (org-googlecl-list-blogs-aux googlecl-blogname
                               googlecl-default-title-filter))

(provide 'org-googlecl)
