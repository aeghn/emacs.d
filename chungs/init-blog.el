(use-package org-static-blog
  :load-path "~/Repos/org-static-blog/"
  :init
  (setq org-static-blog-publish-title "Aeghn"
        org-static-blog-publish-url "https://wangz.me/"
        org-static-blog-publish-directory "~/Repos/blog/"
        org-static-blog-posts-directory "~/Repos/blog/posts/"
        org-static-blog-drafts-directory "~/Repos/blog/drafts/"
        org-static-blog-enable-tags t
        org-export-with-toc t
        org-export-with-section-numbers t
        org-static-blog-use-preview t)
  (setq org-static-blog-page-header
        "<meta name=\"author\" content=\"Aeghn\">
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
<link href= \"static/style.css\" rel=\"stylesheet\" type=\"text/css\" />
<link rel=\"icon\" href=\"static/favicon.ico\">")

  ;; This preamble is inserted at the beginning of the <body> of every page:
  ;;   This particular HTML creates a <div> with a simple linked headline
  (setq org-static-blog-page-preamble
        "<div id=\"blog-nav\">
  <a href=\"/\">WANGZ's blog </a>
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
