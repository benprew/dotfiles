(require 'ox-publish)

(add-to-list 'load-path "~/dotfiles/blog/") ;; adjust path to where you saved blog-tags.el
(require 'blog-tags)

;; Customize the HTML output
(setq org-html-validation-link nil            ;; Don't show validation link
      org-html-head-include-scripts nil       ;; Use our own scripts
      org-html-head-include-default-style nil ;; Use our own styles
      org-html-head "<link rel=\"stylesheet\" href=\"style.css\" />")

;; Hook into publishing pipeline
(add-hook 'org-publish-before-export-hook
          (lambda (filename)
            (let ((pub-dir (org-publish-property :publishing-directory
                                                  (org-publish-get-project-from-filename filename))))
              (my-blog-collect-tags filename pub-dir))))
(add-hook 'org-publish-after-publishing-hook
          (lambda (file output-file)
            (let ((project (org-publish-get-project-from-filename file)))
              (when project
                (let ((pub-dir (org-publish-property :publishing-directory project)))
                  (my-blog-generate-tag-pages pub-dir)
                  (my-blog-generate-index-page pub-dir))))))

(add-to-list 'org-export-filter-final-output-functions
             #'my-blog-insert-tags)

;; Org-publish project definition
(setq org-publish-project-alist
      '(("blog-org"
         :base-directory "~/blog/org"
         :publishing-directory "~/blog/public"
         :recursive t
         :publishing-function org-html-publish-to-html
         :with-author t
         :with-creator nil
         :with-toc nil
         :section-numbers nil
         :time-stamp-file nil
         :html-head-include-default-style nil
         :html-head "<link rel=\"stylesheet\" href=\"style.css\"/>")

        ("blog-static"
         :base-directory "~/blog/org"
         :base-extension "css\\|js\\|png\\|jpg\\|gif"
         :publishing-directory "~/blog/public"
         :recursive t
         :publishing-function org-publish-attachment)

        ("blog" :components ("blog-org" "blog-static"))))
