;;; init-org.el --- Org's settings

(use-package org
  :bind
  (("C-RET" . org-insert-heading-after-current)

   :map org-mode-map
   ("M-h" . chin/delete-blanks))
  :config

  ;;; Look and feel.
  (use-package org-superstar
    :config
    (setq org-superstar-headline-bullets-list '(">"))
    (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

  (use-package valign
    :hook (org-mode-hook . valign-mode))

  (setq org-hide-emphasis-markers nil)
  (setq org-ellipsis " ...")

  (with-eval-after-load 'ox-latex
    ;; http://orgmode.org/worg/org-faq.html#using-xelatex-for-pdf-export
    ;; latexmk runs pdflatex/xelatex (whatever is specified) multiple times
    ;; automatically to resolve the cross-references.
    (setq org-latex-pdf-process '("xelatex -quiet -shell-escape -f %f")
          org-latex-remove-logfiles t
          org-latex-logfiles-extensions '("aux" "bcf" "blg" "fdb_latexmk" "fls"
                                          "figlist" "idx" "log" "nav" "out" "ptc"
                                          "run.xml" "snm" "toc" "vrb" "xdv"))

    (add-to-list 'org-latex-classes
                 '("elegantpaper"
                   "\\documentclass[lang=cn]{elegantpaper}
                 [NO-DEFAULT-PACKAGES]
                 [PACKAGES]
                 [EXTRA]"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (setq org-latex-listings 'minted)
    (add-to-list 'org-latex-packages-alist '("" "minted")))

  ;;; GTD part
  (setq org-log-done 'time)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "STARTED(s)" "WAIT(w)" "|" "DONE(d)" "CANCELED(c)")))

  ;; change the color of the done headline
  (setq org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t)

  (global-set-key (kbd "C-c o a") 'org-agenda)


  (use-package org-capture
    :init
    (setq org-capture-templates
          '(("t" "ToDo" entry (file (expand-file-name "test.org" default-directory))
             "* TODO %^{ToDo}\n  ADDED: %U " :empty-lines 1 :kill-buffer t)
            ("n" "Note" entry (file "~/Documents/orgs/chinbox/inbox.org")
             "* %^{note}\n     %U " :empty-lines 1 :kill-buffer t)))
    (global-set-key (kbd "C-c o c") 'org-capture))

  ;;; Literal programming
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)
     (shell . t)
     (go . t)
     (kotlin . t)
     (js . t)
     (rust . t)))

  ;;; Org-mode enhance tools
  (use-package org-download
    :hook (org-mode-hook . org-download-enable))

  (defun org-docs-insert-image-from-clipboard ()
    "Take a screenshot into a time stamped unique-named file in the
   same directory as the org-buffer and insert a link to this file."
    (interactive)
    (let*  ((pic-dir-name (concat default-directory "pics"))
            (jpg-file-name (concat pic-dir-name "/" (format-time-string "%Y%m%d_%H%M%S.jpg"))))
      (shell-command (concat "mkdir -p " pic-dir-name " && xclip -selection clipboard -t image/png -o >" jpg-file-name))
      (insert (concat "[[file:" jpg-file-name "]]"))))

  (defun org-create-file-and-link-it ()
    (interactive)
    (if (string= major-mode "org-mode")
        (let ((attach-dir (concat buffer-file-name ".d")))
          (when (not (file-exists-p attach-dir))
            (make-directory attach-dir))
          (let* ((src-name (ivy-read "Select file: " (directory-files attach-dir)))
                 (full-name (concat attach-dir "/" src-name)))
            (insert (concat "[[" full-name "][" src-name "]]" ))
            (find-file full-name)))
      (message "Current buffer is not in org-mode.")))

  :custom
  (org-refile-use-cache t)
  (org-refile-file-list (directory-files-recursively "~/Documents/orgs" "\\.org$"))
  (org-refile-targets '((org-refile-file-list . (:maxlevel . 6))))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-refile-use-outline-path t))

(provide 'init-org)
