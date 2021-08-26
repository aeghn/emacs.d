(defvar chin/chinbox-file "~/Documents/orgs/now.org")
(defun find-chinbox-file ()
  (interactive)
  (if (file-exists-p chin/chinbox-file)
      (find-file chin/chinbox-file)
    (with-temp-file chin/chinbox-file
      (insert "#+TODO: TODO STARTED WAIT PAUSED | DONE  CANCELED"))))

(when (string= (getenv "_EMACS_LOAD_MODE_") "chinbox")
  (progn
    (find-chinbox-file)
    (global-set-key (kbd "M-1") find-chinbox-file)))


(provide 'init-chinbox)
