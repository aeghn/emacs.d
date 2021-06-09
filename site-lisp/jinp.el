;;; -*- lexical-binding: t; -*-
;; 
;; Credits:
;; https://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
;; https://emacs.stackexchange.com/questions/22200/how-to-add-a-prefix-key-to-all-keybindings-of-a-specific-mode
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Minor-Modes.html
;;
;;; Code:
;;

;; Jinp buffer jump part.
(defun jinp-buffer-previous ()
  "Goto previous buffer"
  (interactive)
  (previous-buffer)
  (while (string-match "^\*.*" (buffer-name))
    (next-buffer)))

(defun jinp-buffer-next ()
  "Goto next buffer"
  (interactive)
  (next-buffer)
  (while (string-match "^\*.*" (buffer-name))
    (next-buffer)))

(defun jinp-buffer-manager ()
  "Manager buffers"
  (interactive)
  (ivy-switch-buffer))

;; Jinp window manager part.
(defun jinp-buffer-kill ()
  "Kill current buffer"
  (interactive)
  (let* ((buffer (window-buffer))
         (window (get-buffer-window))
         (kill-buffer
          (catch 'kill-buffer-tag
            (dolist (win (window-list))
              (when (and (eq (window-buffer win) buffer)
                         (not (eq win window)))
                (throw 'kill-buffer-tag nil)))
            (throw 'kill-buffer-tag t))))
    (if kill-buffer
        (progn (message "buffer `%s' is killed" buffer)
               (kill-current-buffer))
      (progn (message "window `%s' is deleted" window)
             (delete-window)))))

(defun jinp-window-delete ()
  "Close this window"
  (interactive)
  (delete-window))

(defun jinp-window-move-up ()
  "Move current window up"
  (interactive)
  (windmove-swap-states-up))

(defun jinp-window-move-down ()
  "Move current window donw"
  (interactive)
  (windmove-swap-states-down))

(defun jinp-window-move-left ()
  "Move current window left"
  (interactive)
  (windmove-swap-states-left))

(defun jinp-window-move-right ()
  "Move current window right"
  (interactive)
  (windmove-swap-states-right))

;; Jinp mark jump part.
(defun jinp-mark-browser ()
  "Browser all marks"
  (interactive)
  (counsel-bookmark))

;; Jinp jump lines and words
(defun jinp-line-jump ()
  "Jump into lines with number."
  (interactive)
  (let ((line-num (read-string "Jump to line: ")))
    (goto-char (point-min))
    (forward-line (1- (string-to-number line-num)))))

(defun jinp-line-kick ()
  "Jump into lines with number."
  (interactive)
  (avy-goto-word-1 (read-char "char: " t)))

(defun jinp-line-head ()
  "Jump to the first line."
  (interactive)
  (goto-char (point-min)))

(defun jinp-line-tail ()
  "Jump to the last line."
  (interactive)
  (goto-char (point-max)))

(defalias 'jinp-mode-map (make-sparse-keymap))
(defvar jinp-mode-map (symbol-function 'jinp-mode-map)
  "Global keymap for characters following C-z.")

(global-unset-key (kbd "M-j"))
(define-key global-map (kbd "M-j") 'jinp-mode-map)

(define-key jinp-mode-map (kbd "M-p") 'jinp-buffer-previous)
(define-key jinp-mode-map (kbd "M-n") 'jinp-buffer-next)
(define-key jinp-mode-map (kbd "M-u") 'jinp-buffer-kill)
(define-key jinp-mode-map (kbd "M-d") 'jinp-window-delete)
(define-key jinp-mode-map (kbd "M-l") 'jinp-line-kick)
(define-key jinp-mode-map (kbd "M-j") 'jinp-line-jump)
(define-key jinp-mode-map (kbd "M-,") 'jinp-line-head)
(define-key jinp-mode-map (kbd "M-.") 'jinp-line-tail)
(define-key jinp-mode-map (kbd "M-k") 'jinp-buffer-manager)
(define-key jinp-mode-map (kbd "M-m") 'jinp-mark-browser)
(define-key jinp-mode-map (kbd "o") 'jinp-window-move-right)

(provide 'jinp)
