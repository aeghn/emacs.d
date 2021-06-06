(defun goto-match-paren ()
  "Go to the matching parenthesis if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive)
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t)))

(global-set-key (kbd "C-'") 'goto-match-paren)

(defun indent-current-buffer ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defcustom chin/indent-sensitive-modes
  '(coffee-mode python-mode slim-mode haml-mode yaml-mode)
  "Modes for which auto-indenting is suppressed."
  :type 'list)

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (unless (member major-mode chin/indent-sensitive-modes)
    (save-excursion
      (if (region-active-p)
          (progn
            (indent-region (region-beginning) (region-end))
            (untabify (region-beginning) (region-end))
            (message "Indented selected region."))
        (progn
          (indent-current-buffer)
          (message "Indented buffer.")))
      (whitespace-cleanup))))

(global-set-key (kbd "C-c i") 'indent-region-or-buffer)

;; recentf
(use-package recentf
  :ensure nil
  ;; :bind (("C-x C-r" . recentf-open-files))
  :hook (after-init . recentf-mode)
  :init
  (setq recentf-max-saved-items 5000)
  :config
  (recentf-track-opened-file))

(use-package helpful
  :commands helpful--buffer
  :bind
  (("C-c C-d" . helpful-at-point)
   (:map help-map
         ("k" . helpful-key)
         ("F" . helpful-command)
         ("f" . helpful-callable)
         ("v" . helpful-variable))))

(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; (global-display-line-numbers-mode -1)

(use-package windmove
  :bind
  ("<H-up>" . windmove-up)
  ("<H-down>" . windmove-down)
  ("<H-left>" . windmove-left)
  ("<H-right>" . windmove-right))

;; Expand region
(use-package expand-region
  :init
  (require 'expand-region)
  :bind (("C-=" . er/expand-region))
  :config
  (define-advice set-mark-command (:before-while (arg))
    "Repeat C-SPC to expand region."
    (interactive "P")
    (if (eq last-command 'set-mark-command)
        (progn
          (er/expand-region 1)
          nil)
      t)))

;; Hideshow
(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  :bind (:map hs-minor-mode-map
              ("C-`" . hs-toggle-hiding)))

(use-package treemacs
  :config
  (setq treemacs-no-png-images t))
;; (global-hl-line-mode 1)


;; Project management
(use-package projectile
  :ensure t
  :hook (after-init . projectile-mode)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  (defconst projectile-ignored-project-directories `("/tmp/"
                                                     "/private/tmp/"))
  (defun projectile-project-ignore-p (file)
    (cl-loop for ig-dir in projectile-ignored-project-directories
             when (string-prefix-p ig-dir file)
             return t)
    )

  (dolist (dir '("bazel-bin"
                 "bazel-out"
                 "bazel-testlogs"))
    (add-to-list 'projectile-globally-ignored-directories dir))
  (dolist (file '(".DS_Store"))
    (add-to-list 'projectile-globally-ignored-files file))
  (setq projectile-project-root-files (append '("go.mod") projectile-project-root-files))
  :custom
  (projectile-use-git-grep t)
  (projectile-completion-system 'ivy)
  (projectile-indexing-method 'alien)
  (projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o" ".swp" ".so"))
  (projectile-ignored-project-function 'projectile-project-ignore-p))

;;; init-default.el ends here
(provide 'init-default)
