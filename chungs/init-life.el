(use-package beancount
  :defer t
  :bind
  (:map beancount-mode-map
        ("M-RET" . beancount-insert-account-ivy)
        ("C-<return>" . beancount-insert-date))
  :mode ("\\.bean\\(?:count\\)?\\'" . beancount-mode)
  :hook (beancount-mode . yas-minor-mode-on)
  :config
  (defun beancount-collect (regexp n filename)
    "Return an unique list of REGEXP group N in the current buffer."
    (with-temp-buffer
      (insert-file-contents filename)
      (save-excursion
        (save-match-data
          (let ((hash (make-hash-table :test 'equal)))
            (goto-char (point-min))
            (while (re-search-forward regexp nil t)
              ;; Ignore matches around `pos' (the point position when
              ;; entering this funcyion) since that's presumably what
              ;; we're currently trying to complete.
              (puthash (match-string-no-properties n) nil hash))
            (hash-table-keys hash))))))

  (defun beancount-insert-account-ivy ()
    (interactive)
    (let* ((beancount-accounts-list
            (beancount-collect beancount-account-regexp 0 "~/Documents/beancount/accounts.bean"))
           (beancount-account (ivy-read "Account: " beancount-accounts-list )))
      (insert (concat beancount-account "  CNY"))
      (search-backward " CNY"))))

(provide 'init-life)
