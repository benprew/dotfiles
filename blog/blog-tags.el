;;; blog-tags.el --- Tag support for Org-publish blog

(defvar my-blog-tags-table (make-hash-table :test 'equal)
  "Hash table mapping tag -> list of posts.")

(defvar my-blog-posts-table (make-hash-table :test 'equal)
  "Hash table storing all post metadata for index generation.")

(defvar my-blog-index-config
  '((most-recent . ((sort . date) (limit . 5) (title . "Most Recent")))
    (value-per-word . ((manual . t) (title . "Value Per Word")
                       (description . "Short, impactful articles")))
    (most-important . ((manual . t) (title . "Most Important")
                       (description . "Essential reads")))
    (most-popular . ((sort . views) (limit . 6) (title . "Most Popular"))))
  "Configuration for index page sections.")

(defun my-blog-collect-tags (filename pub-dir)
  "Collect tags and metadata from FILENAME into both tables."
  (with-current-buffer (find-file-noselect filename)
    (let* ((keywords (org-collect-keywords '("TAGS" "TITLE" "DATE" "CATEGORY" "POPULAR" "IMPORTANT" "VALUE_PER_WORD")))
           (tags (cdr (assoc "TAGS" keywords)))
           (title (cadr (assoc "TITLE" keywords)))
           (date (cadr (assoc "DATE" keywords)))
           (category (cadr (assoc "CATEGORY" keywords)))
           (popular (cadr (assoc "POPULAR" keywords)))
           (important (cadr (assoc "IMPORTANT" keywords)))
           (value-per-word (cadr (assoc "VALUE_PER_WORD" keywords)))
           (output (concat (file-name-sans-extension
                            (file-name-nondirectory filename)) ".html"))
           (word-count (count-words (point-min) (point-max)))
           (post-data (list :title title :file output :date date
                           :tags (and tags (split-string (car tags) " " t))
                           :category category :word-count word-count
                           :popular popular :important important
                           :value-per-word value-per-word)))

      ;; Store in posts table
      (puthash output post-data my-blog-posts-table)

      ;; Store in tags table
      (dolist (tag (and tags (split-string (car tags) " " t)))
        (puthash tag
                 (cons (list :title title :file output :date date)
                       (gethash tag my-blog-tags-table))
                 my-blog-tags-table)))))

(defun my-blog-generate-tag-pages (pub-dir)
  "Generate one HTML page per tag in PUB-DIR."
  (maphash
   (lambda (tag posts)
     (with-temp-file (expand-file-name (concat tag ".html") pub-dir)
       (insert "<html><head>")
       (insert (format "<title>Posts tagged %s</title>" tag))
       (insert "<link rel=\"stylesheet\" href=\"style.css\">")
       (insert "</head><body>")
       (insert (format "<h1>Posts tagged: %s</h1><ul>" tag))
       (dolist (post (reverse posts)) ;; newest first
         (insert (format "<li><a href=\"%s\">%s</a></li>"
                         (plist-get post :file)
                         (plist-get post :title))))
       (insert "</ul></body></html>")))
   my-blog-tags-table))

(defun my-blog-insert-tags (output backend info)
  "Prepend a list of tags as links to the top of a post during HTML export."
  (when (org-export-derived-backend-p backend 'html)
    (let ((tags (cdr (assoc "TAGS" (org-collect-keywords '("TAGS"))))))
      (if tags
          (let ((tag-html (concat "<p><strong>Tags:</strong> "
                                  (mapconcat
                                   (lambda (tag)
                                     (format "<a href=\"%s.html\">%s</a>" tag tag))
                                   (split-string (car tags) " " t)
                                   " ")
                                  "</p>\n")))
            (if (string-match "\\(<h1[^>]*>.*?</h1>\\)" output)
                (replace-match (concat "\\1\n" tag-html) nil nil output)
              (concat output tag-html)))
        output))))

(defun my-blog-sort-posts (posts sort-key)
  "Sort POSTS by SORT-KEY (date, word-count, etc.)."
  (sort posts
        (lambda (a b)
          (cond
           ((eq sort-key 'date)
            (let ((date-a (plist-get a :date))
                  (date-b (plist-get b :date)))
              (string> (or date-a "") (or date-b ""))))
           ((eq sort-key 'word-count)
            (< (or (plist-get a :word-count) 0)
               (or (plist-get b :word-count) 0)))
           ((eq sort-key 'title)
            (string< (or (plist-get a :title) "")
                     (or (plist-get b :title) "")))
           (t nil)))))

(defun my-blog-filter-posts (posts filter-key)
  "Filter POSTS by FILTER-KEY (important, popular, value-per-word)."
  (seq-filter (lambda (post)
                (plist-get post filter-key))
              posts))

(defun my-blog-generate-index-section (section-key config posts)
  "Generate HTML for one index section."
  (let* ((title (cdr (assoc 'title config)))
         (description (cdr (assoc 'description config)))
         (manual (cdr (assoc 'manual config)))
         (sort-key (cdr (assoc 'sort config)))
         (limit (cdr (assoc 'limit config)))
         (filtered-posts (if manual
                           (my-blog-filter-posts posts section-key)
                           posts))
         (sorted-posts (if sort-key
                         (my-blog-sort-posts filtered-posts sort-key)
                         filtered-posts))
         (limited-posts (if limit
                          (seq-take sorted-posts limit)
                          sorted-posts)))
    (concat
     (format "<section class=\"index-section %s\">\n" (symbol-name section-key))
     (format "<h2>%s</h2>\n" title)
     (when description
       (format "<p class=\"section-description\">%s</p>\n" description))
     "<ul class=\"post-list\">\n"
     (mapconcat
      (lambda (post)
        (format "<li><span class=\"date\">%s</span> <a href=\"%s\">%s</a> %s</li>\n"
                (or (plist-get post :date) "")
                (plist-get post :file)
                (plist-get post :title)
                (if (plist-get post :tags)
                    (concat "<span class=\"tags\">"
                            (mapconcat (lambda (tag)
                                        (format "<a href=\"%s.html\">%s</a>" tag tag))
                                      (plist-get post :tags) " ")
                            "</span>")
                  "")))
      limited-posts "")
     "</ul>\n"
     "</section>\n")))

(defun my-blog-populate-tables-from-directory (pub-dir)
  "Populate tables by scanning org files in the source directory."
  (let ((org-dir (replace-regexp-in-string "/public$" "/org" pub-dir)))
    (dolist (file (directory-files org-dir t "\\.org$"))
      (my-blog-collect-tags file pub-dir))))

(defun my-blog-generate-index-page (pub-dir)
  "Generate the main index.html page."
  ;; Ensure tables are populated
  (my-blog-populate-tables-from-directory pub-dir)

  (let ((all-posts (let (posts)
                     (maphash (lambda (file data)
                               (push data posts))
                              my-blog-posts-table)
                     posts)))
    (with-temp-file (expand-file-name "index.html" pub-dir)
      (insert "<!DOCTYPE html>\n")
      (insert "<html>\n<head>\n")
      (insert "<meta charset=\"utf-8\">\n")
      (insert "<title>Blog Index</title>\n")
      (insert "<link rel=\"stylesheet\" href=\"style.css\">\n")
      (insert "</head>\n<body>\n")
      (insert "<div id=\"content\" class=\"content\">\n")
      (insert "<h1>Blog Index</h1>\n")

      ;; Generate each section
      (dolist (section-config my-blog-index-config)
        (let ((section-key (car section-config))
              (config (cdr section-config)))
          (insert (my-blog-generate-index-section section-key config all-posts))))

      ;; Add tags section
      (insert "<section class=\"index-section tags\">\n")
      (insert "<h2>Tags</h2>\n")
      (insert "<div class=\"tag-cloud\">\n")
      (maphash (lambda (tag posts)
                 (insert (format "<a href=\"%s.html\" class=\"tag\">%s (%d)</a> "
                               tag tag (length posts))))
               my-blog-tags-table)
      (insert "</div>\n</section>\n")

      (insert "</div>\n</body>\n</html>\n"))))

(provide 'blog-tags)
