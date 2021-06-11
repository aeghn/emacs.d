(use-package dired
  :bind (:map dired-mode-map
              ("N" . chin/dired-create-file)
              )
  :config
  (defun chin/dired-create-file (file)
    "Create a file called FILE.
If FILE already exists, signal an error."
    (interactive
    (list (read-file-name "Create file: " (dired-current-directory))))
  (let* ((expanded (expand-file-name file))
         (try expanded)
         (dir (directory-file-name (file-name-directory expanded)))
         new)
    (if (file-exists-p expanded)
        (error "Cannot create file %s: file exists" expanded))
    ;; Find the topmost nonexistent parent dir (variable `new')
    (while (and try (not (file-exists-p try)) (not (equal new try)))
      (setq new try
            try (directory-file-name (file-name-directory try))))
    (when (not (file-exists-p dir))
      (make-directory dir t))
    (write-region "" nil expanded t)
    (when new
      (dired-add-file new)
      (dired-move-to-filename))))

(defadvice dired-find-file (around dired-find-file-single-buffer activate)
  "Replace current buffer if file is a directory."
  (interactive)
  (let ((orig (current-buffer)) (filename (dired-get-file-for-visit)))
    ad-do-it (when (and (file-directory-p filename) (not (eq (current-buffer) orig)))
               (kill-buffer orig)))))



(provide 'init-dired)
