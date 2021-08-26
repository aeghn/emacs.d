;; font settings.
(defun chin/set-font ()
  (set-face-attribute
   'default nil
   :font "Roboto Mono-11")

  (set-face-attribute
   'mode-line nil
   :font "Roboto-12")

  (set-face-attribute
   'mode-line-inactive nil
   :font "Roboto-12")

  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font
     (frame-parameter nil 'font)
     charset
     (font-spec :family "STHeiti"
                :weight 'normal)))
  (use-cjk-char-width-table 'zh_CN)

  (set-frame-parameter (selected-frame)
                       'internal-border-width 10)

  (setq-default left-margin-width 0 right-margin-width 2)
  (set-window-margins nil 0 0)

  (setq face-font-rescale-alist '(("STHeiti" . 1.0))))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (if (display-graphic-p)
                (chin/set-font))))

(setq-default overline-margin 2
              ;; This fix the cursor movement lag
              auto-window-vscroll nil
              underline-minimum-offset 10)


(setq inhibit-compacting-font-caches t)

;; Backup file and auto save default files
(setq-default make-backup-files nil
              history-length 1000)

(setq auto-save-default nil
      save-silently t
      auto-save-list-file-prefix (expand-file-name
                                  "auto-save-list/.save-"
                                  chin/temporary-files-directory))

(setq browse-url-browser-function 'browse-url-default-browser)

;; Set tab width
(setq-default tab-width 4)
;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; Change all yes-or-no to y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

(require 'package)
(setq-default tsinghua-mirror
              '(("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                ("org"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

;; 猪场
(setq-default netease-mirror
              '(("melpa" . "http://mirrors.163.com/elpa/melpa/")
                ("org"   . "http://mirrors.163.com/elpa/org/")
                ("gnu"   . "http://mirrors.163.com/elpa/gnu/")))

;; 鹅厂
(setq-default tecent-mirror
              '(("melpa" . "http://mirrors.cloud.tencent.com/elpa/melpa/")
                ("org"   . "http://mirrors.cloud.tencent.com/elpa/org/")
                ("gnu"   . "http://mirrors.cloud.tencent.com/elpa/gnu/")))

(setq package-archives tsinghua-mirror)

(package-initialize)

;; (setq use-package-always-ensure t)
;; (setq use-package-always-defer t)
(require 'use-package)

(use-package no-littering
  :demand t
  :init
  (setq no-littering-etc-directory chin/temporary-files-directory)
  (setq no-littering-var-directory chin/temporary-files-directory)
  (require 'no-littering))

(defun chin/move-beginning-of-line ()
  "Move point back to indentation of beginning of line or beginning of line."
  (interactive)
  (let ((orig-begin (point)))
    (back-to-indentation)
    (if (= orig-begin (point))
        (beginning-of-line))))

(defun server-shutdown ()
  "Save buffers, quit, and shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(defun chin/revert-buffer ()
  "Revert buffer without confirming."
  (interactive)
  (revert-buffer t t t)
  (message "buffer is reverted"))

(defun my-insert-tab-char ()
  "Insert a tab char. (ASCII 9, \t)"
  (interactive)
  (insert "\t"))

;; bindings related to mode-line
(global-unset-key (kbd "M-m"))
(define-prefix-command 'chin/private-map)
(global-set-key (kbd "M-m") 'chin/private-map)
(global-set-key (kbd "M-m m f")
                (lambda ()
                  (interactive)
                  (message "Current file is: %s" buffer-file-name)))

(defun chin/open-external ()
  "Open current buffer file in external soft."
  (interactive)
  (let ((file-name buffer-file-name))
    (if (string= file-name "nil")
        (message ">>> Current buffer `%s' has no file." (buffer-name))
      (let ((command (ivy-read "Select external app: " (directory-files "/bin"))))
        (call-process-shell-command  (concat command " '" file-name "' &!") nil 0)
        (message ">>> Opened `%s' by %s." file-name command)))))

(defun chin/open-scratch ()
  (interactive)

  )

;; Reference: view-echo-area-messages
(global-set-key (kbd "M-m n")
                (lambda ()
                  (interactive)
                  (with-current-buffer (messages-buffer)
                    (goto-char (point-max))
                    (let ((win (display-buffer (current-buffer))))
                      (select-window win)))))

(bind-key "C-a" 'chin/move-beginning-of-line)
(bind-key "C-h" 'backward-delete-char-untabify)
(bind-key "C-c h" 'help-command)
(bind-key "M-h" 'backward-kill-word)
(bind-key "C-c C-r" 'chin/revert-buffer)
(bind-key "C-;" 'comment-dwim-2)
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "<f2>") (lambda() (interactive)(find-file "~/.emacs.d/timeline.org")))
(defun chin/delete-blanks ()
  (interactive)
  (let* ((end-pos (progn (back-to-indentation)
                         (point)))
         (start-pos (1+ (search-backward-regexp "[^\n[:space:]]"))))
    (delete-region start-pos end-pos)
    (forward-char)
    (insert " ")))

(bind-key "M-h" 'chin/delete-blanks)



(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets" chin/library-files-directory))
  :hook
  (after-init . yas-global-mode)
  :config
  (require 'warnings)
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change))
  )

;;; init-default.el ends here
(provide 'init-basic)
