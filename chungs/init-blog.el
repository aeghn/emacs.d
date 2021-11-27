(use-package org-static-blog
  :load-path "~/Repos/org-static-blog/"
  :init
  (setq org-static-blog-publish-title "wangz.me"
        org-static-blog-publish-url "https://wangz.me/"
        org-static-blog-publish-directory "~/Repos/blog"
        org-static-blog-posts-directory "~/Repos/blog/posts/"
        org-static-blog-drafts-directory "~/Repos/blog/drafts/"
        org-static-blog-index-file "index.html"
        org-static-blog-archive-file "index.html"
        org-static-blog-langcode "zh"
        org-static-blog-enable-tags t
        org-export-with-toc t
        org-export-with-section-numbers t
        org-static-blog-use-preview t)

  (defun chin/org-static-blog-insert-date ()
    (interactive)
    (goto-char (point-min))
    (when (and (not (search-forward-regexp "^\\#\\+date:[ ]*<\\([^]>]+\\)>$" nil t))
               (eq (buffer-local-value 'major-mode (current-buffer)) 'org-mode))
      (progn
        (goto-char (point-min))
        (forward-line)
        (beginning-of-line)
        (insert "#+date: ")
        (org-time-stamp nil)
        (newline))))


  (defun chin/org-static-blog-link-buffer ()
    (interactive)
    (let* ((real-file-path (buffer-file-name))
           (file-name (buffer-name))
           (symbolic-file-path (expand-file-name file-name org-static-blog-posts-directory)))
      (message "try to make link: %s -> %s" symbolic-file-path real-file-path)
      (make-symbolic-link real-file-path symbolic-file-path t)))

  (setq org-static-blog-page-header
        "<meta name=\"author\" content=\"Aeghn\">
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
<link href= \"static/style.css\" rel=\"stylesheet\" type=\"text/css\" />
<link rel=\"icon\" href=\"static/favicon.ico\">")

  ;; This preamble is inserted at the beginning of the <body> of every page:
  ;;   This particular HTML creates a <div> with a simple linked headline
  (setq org-static-blog-page-preamble
        "<div id=\"blog-nav\">
  <a href=\"/\">wangz's blog </a>
  <span>/ </span>
  <a href=\"/tags.html\">Tags </a>
</div><hr/>")

  ;; This postamble is inserted at the end of the <body> of every page:
  ;;   This particular HTML creates a <div> with a link to the archive page
  ;;   and a licensing stub.
  (setq org-static-blog-page-postamble
        "\n<div id=\"blog-tail\">
<center><span> Powered by Org-mode and</span> <a href=\"https://github.com/aeghn/org-static-blog\"> a fork</a><span> of</span><a href=\"https://github.com/bastibe/org-static-blog\"> org-static-blog</a> </center>
</div>"))

(provide 'init-blog)
