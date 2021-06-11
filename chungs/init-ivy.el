(require 'package)
(require 'counsel)

(use-package counsel
  :hook ((after-init . ivy-mode)
         (ivy-mode   . counsel-mode))
  :bind ( ("C-s"   . swiper-isearch)
          ("C-r"   . swiper-isearch-backward)
          ("C-S-f"   . swiper)
          ("C-S-s" . swiper-all)

          :map counsel-mode-map
          ([remap swiper] . counsel-grep-or-swiper)
          ([remap swiper-backward] . counsel-grep-or-swiper-backward)
          ([remap dired] . counsel-dired)
          ([remap set-variable] . counsel-set-variable)
          ([remap insert-char] . counsel-unicode-char)
          ([remap recentf-open-files] . counsel-recentf)
          ("C-c c r" . counsel-rg-with-prompt)
          ("C-c c f" . counsel-fzf-with-prompt)

          :map counsel-find-file-map
          ("C-u" . counsel-up-directory)

          :map swiper-map
          ("M-s" . swiper-isearch-toggle)
          ("M-%" . swiper-query-replace)

          :map isearch-mode-map
          ("M-s" . swiper-isearch-toggle)

          :map ivy-minibuffer-map
          ("M-j" . pyim-convert-string-at-point)
          )

  :init (setq enable-recursive-minibuffers t) ; Allow commands in minibuffers

  (setq ivy-use-selectable-prompt t
        ivy-use-virtual-buffers t    ; Enable bookmarks and recentf
        ivy-height 10
        ivy-fixed-height-minibuffer t
        ivy-count-format "(%d/%d) "
        ivy-on-del-error-function nil
        ivy-initial-inputs-alist nil
        ;; Show full history file name
        ivy-virtual-abbreviate 'abbreviate)
  ;; )

  (setq swiper-action-recenter t)

  (use-package amx
    :init (setq amx-history-length 20)
    )
  :config
  (defun counsel-rg-with-prompt ()
    "A wrapper for counsel-rg which shows search dir."
    (interactive)
    (let ((initial-directory default-directory))
      (when current-prefix-arg
        (setq initial-directory
              (counsel-read-directory-name " rg in directory: ")))
      (counsel-rg nil initial-directory nil (concat "Search " initial-directory " for: "))))

  (defun counsel-fzf-with-prompt ()
    "A wrapper for counsel-fzf which shows search dir."
    (interactive)
    (let ((fzf-basename (car (split-string counsel-fzf-cmd)))
          (initial-directory default-directory))
      (when current-prefix-arg
        (setq initial-directory
              (counsel-read-directory-name (concat
                                            fzf-basename
                                            " in directory: "))))
      (counsel-fzf nil initial-directory (concat "Search " initial-directory " for: "))))
  (defun chin/edit-shell-script ()
    "Create bash script in ~/.scripts/"
    (interactive)
    (let* ((script-dir (expand-file-name ".scripts" (file-truename "~")))
           (script-name (ivy-read "Select file: " (directory-files script-dir)))
           (script-path (expand-file-name script-name script-dir)))
      (shell-command (concat "touch " script-path "; chmod +x " script-path))
      (find-file script-path)
      (shell-script-mode)
      (when (= (buffer-size) 0)
        (insert "#!/usr/bin/env bash\n\n# ")))))

(use-package pinyinlib
  :commands pinyinlib-build-regexp-string
  :init
  (with-no-warnings
    (defun ivy--regex-pinyin (str)
      "The regex builder wrapper to support pinyin."
      (or (pinyin-to-utf8 str)
          (and (fboundp 'ivy-prescient-non-fuzzy)
               (ivy-prescient-non-fuzzy str))
          (ivy--regex-plus str)))

    (defun my-pinyinlib-build-regexp-string (str)
      "Build a pinyin regexp sequence from STR."
      (cond ((equal str ".*") ".*")
            (t (pinyinlib-build-regexp-string str t))))

    (defun my-pinyin-regexp-helper (str)
      "Construct pinyin regexp for STR."
      (cond ((equal str " ") ".*")
            ((equal str "") nil)
            (t str)))

    (defun pinyin-to-utf8 (str)
      "Convert STR to UTF-8."
      (cond ((equal 0 (length str)) nil)
            ((equal (substring str 0 1) "!")
             (mapconcat
              #'my-pinyinlib-build-regexp-string
              (remove nil (mapcar
                           #'my-pinyin-regexp-helper
                           (split-string
                            (replace-regexp-in-string "!" "" str )
                            "")))
              ""))
            (t nil)))

    (mapcar
     (lambda (item)
       (let ((key (car item))
             (value (cdr item)))
         (when (member value '(ivy-prescient-non-fuzzy
                               ivy--regex-plus))
           (setf (alist-get key ivy-re-builders-alist)
                 #'ivy--regex-pinyin))))
     ivy-re-builders-alist)))

(use-package ivy-rich
  :init
  (defun ivy-rich-bookmark-name (candidate)
    "Return bookmark name from CANDIDATE."
    (car (assoc candidate bookmark-alist)))

  (ivy-rich-mode 0)
  (setq ivy-rich-display-transformers-list
        (plist-put ivy-rich-display-transformers-list
                   'counsel-bookmark
                   '(:columns
                     ((ivy-rich-bookmark-name (:width 40))
                      (ivy-rich-bookmark-info (:width 40)))
                     :delimiter "\t")))
  (ivy-rich-mode 1)
  (setq ivy-virtual-abbreviate 'full
        ivy-rich-switch-buffer-align-virtual-buffer t
        ivy-rich-parse-remote-buffer nil   ;; For better performance
        ivy-rich-path-style 'abbrev))



;;; init-keys.el ends here
(provide 'init-ivy)
