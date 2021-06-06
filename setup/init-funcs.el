(defvar litter-dir (file-truename "~/Litter/"))

(defun chin/setup-new-script ()
  (interactive)
  (let* ((name-prefix (read-string "Name prefix: "))
         (file-name (concat name-prefix (format-time-string "_%Y%m%d_%H%M.sh")))
         (file-path (expand-file-name file-name litter-dir)))

    (write-region "#!/usr/bin/env bash\n\n#" nil file-path)
    (shell-command (concat "chmod +x " file-path))
    (find-file file-path)))

(defun count-ce-words (beg end)
  "Count Chinese and English words in marked region."
  (interactive "r")
  (let ((cn-word 0)
        (en-word 0)
        (total-word 0)
        (total-byte 0))
    (setq cn-word (count-matches "\\cc" beg end)
          en-word (count-matches "\\w+\\W" beg end))
    (setq total-word (+ cn-word en-word)
          total-byte (+ cn-word (abs (- beg end))))
    (message (format "Total: %d (CN: %d, EN: %d) words, %d bytes."
                     total-word cn-word en-word total-byte))))


(defun chin/today-file ()
  "Create bash script in ~/Workspace/scripts/"
  (interactive)
  (let ((today-dir (expand-file-name (format-time-string "%m%d") litter-dir)))
    (unless (file-exists-p today-dir)
      (make-directory today-dir))
    (let*
        ((file-name (ivy-read (concat "Select file(" today-dir "): ")  (directory-files today-dir)))
         (file-path (expand-file-name file-name today-dir)))
      (find-file file-path))))


(provide 'init-funcs)
