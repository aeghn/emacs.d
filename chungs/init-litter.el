(defun work/org-export-docx ()
  (interactive)
  (let ((docx-file (concat (file-name-sans-extension (buffer-file-name)) ".docx"))
        (template-file (file-truename "~/.emacs.d/library/others/template.docx")))
    (if (file-exists-p template-file)
                       (progn
                         (shell-command (format "pandoc %s -o %s --reference-doc=%s  --toc --wrap=preserve" (buffer-file-name) docx-file template-file))
                         (message "Convert maybe finished: %s" docx-file))
      (message "Error: %s didn't exist" template-file))))
