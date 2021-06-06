;;; init-shell.el --- All about shell not shell scripts -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(eval-when-compile
  (require 'rx))

(defun term-mode-common-init ()
  "The common initialization for term."
  (setq-local scroll-margin 0)
  (setq-local truncate-lines t)
  (setq-local global-hl-line-mode nil))

  ;; General term mode
  ;;
  ;; If you use bash, directory track is supported natively.
  ;; See https://www.emacswiki.org/emacs/AnsiTermHints for more information.
  (use-package term
    :ensure nil
    :hook (term-mode . (lambda ()
                         (term-mode-common-init)
                         (when-let* ((proc (ignore-errors (get-buffer-process (current-buffer)))))
                           ;; Don't prompt about processes when killing term
                           (set-process-query-on-exit-flag proc nil))))
    :bind (:map term-raw-map
                ("C-c C-y" . term-paste)
                ;; Don't capture my keys!
                ("M-o" . nil)
                ("M-j" . nil)
                ("M-:" . nil)
                ("M-x" . nil)
                ("C-h" . nil)
                ("C-u" . nil))
    :custom
    (term-input-ignoredups t)
    (term-completion-autolist t)
    (term-scroll-to-bottom-on-output 'all))

;; Popup a shell inside Emacs
(use-package shell-pop
  :ensure t
  :custom
  (shell-pop-universal-key "M-=")
  (shell-pop-full-span t)
  (shell-pop-window-size 40)
  (shell-pop-shell-type (if (not (eq system-type 'windows-nt))
                            '("ansi-term" "*ansi-term*" (lambda () (ansi-term shell-pop-term-shell)))
                          '("eshell" "*eshell*" #'eshell))))

(provide 'init-term)
