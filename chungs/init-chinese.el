(use-package posframe
  :init (require 'posframe))

;; (use-package pyim
;;   :demand t
;;   :config
;;   (if (display-graphic-p)
;;       (setq pyim-page-tooltip 'posframe)
;;     (setq pyim-page-tooltip 'popup))

  
;;   (setq pyim-default-scheme 'quanpin)

;;   ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
;;   ;; 我自己使用的中英文动态切换规则是：
;;   ;; 1. 光标只有在注释里面时，才可以输入中文。
;;   ;; 2. 光标前是汉字字符时，才能输入中文。
;;   ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
;;   (setq-default pyim-english-input-switch-functions
;;                 '(pyim-probe-dynamic-english
;;                   pyim-probe-isearch-mode
;;                   pyim-probe-program-mode
;;                   pyim-probe-org-structure-template))

;;   (setq-default pyim-punctuation-half-width-functions
;;                 '(pyim-probe-punctuation-line-beginning
;;                   pyim-probe-punctuation-after-punctuation))

;;   ;; 开启拼音搜索功能
;;   ;;(pyim-isearch-mode 1)

;;   ;; 选词框显示5个候选词
;;   (setq pyim-page-length 5)

;;   :bind
;;   (("M-i" . pyim-convert-string-at-point) ;与 pyim-probe-dynamic-english 配合

;;    :map pyim-mode-map
;;    ("C-h" . pyim-entered-delete-backward-char)
;;    ))

;; (use-package liberime
;;   :init
;;   (setq liberime-user-data-dir (expand-file-name "rime" chin/temporary-files-directory ))
;;   (setq liberime-shared-data-dir (expand-file-name "~/.config/fcitx/rime/"))
;;   (add-hook 'liberime-after-start-hook
;;             (lambda () (liberime-select-schema "double_pinyin_flypy")))
;;   :config
;;   (unless (file-exists-p (concat (liberime-get-library-directory)
;;                                  "build/liberime-core"
;;                                  module-file-suffix))
;;     (liberime-build)))

(use-package rime
  :bind
  ;; (("M-j" . rime-force-enable)

  (("M-m c" . rime-switch-mode)
   ("M-c" . rime-change-state-manually)
   :map rime-active-mode-map
   ("TAB" . rime-inline-ascii))
  :custom
  (rime-emacs-module-header-root "~/.local/include")

  :config
  (setq rime-user-data-dir "~/.emacs.d/library/rime")

  (setq rime-translate-keybindings
        '("C-f" "C-b" "C-n" "C-p" "C-g"))

  (setq default-input-method "rime"
        rime-show-candidate 'posframe
        rime-posframe-style 'simple
        rime-show-preedit t)

  (defvar rime-input-mode nil)
  (setq rime-posframe-properties
        (list :font "AMS"
              :internal-border-width 10))

  (setq rime-disable-predicates
        '(rime-english-mode))

  ;; nil for predicates
  ;; (setq-default rime-manual-lang nil)

  (defun rime-after-change-function (beg end len)
    "Add to hook `after-change-functions', in order to unbound `rime-manual-lang'"
    (let ((inhibit-modification-hooks nil))
      (makunbound 'rime-manual-lang)))

  (defun rime-start ()
    "If current input method is not rime, enable it and add `rime-after-change-function' hook"
    (interactive)
    (unless (member 'rime-after-change-function after-change-functions)
      (add-hook 'after-change-functions 'rime-after-change-function))
    (unless (string= current-input-method "rime")
      (activate-input-method 'rime)))

  (defun rime-stop ()
    "If current input method is rime, disable it and remove `rime-after-change-function' hook"
    (interactive)
    (when (member 'rime-after-change-function after-change-functions)
      (remove-hook 'after-change-functions 'rime-after-change-function))
    (when (string= current-input-method "rime")
      (rime-mode -1)))

  ;; Use English if return t,
  ;; use Chinese if return nil.
  (defun rime-english-mode ()
    "Start new line with English.
Chinnnnnnnese followed by Chinese.
Space followed by English."
    (cond
     ((boundp 'rime-manual-lang) rime-manual-lang)
     (t (not (looking-back "\\cc" 1)))))

  (defun rime-chinese-mode ()
    "Start new line with Chinese.
Chinese followed by Chinese.
English followed by English.
Space followed by English."
    (cond
     ((boundp 'rime-manual-lang) rime-manual-lang)
     ((and (org-in-src-block-p) (not (looking-back "\\cc" 1))) t)
     ((> (point) (save-excursion (back-to-indentation) (point)))
      (if (looking-back " +" 1)
          (looking-back "\\cc +" 2)
        (not (looking-back "\\cc" 1))))))

  (defun rime-switch-mode ()
    "Switch between Chinese and English modes."
    (interactive)
    (rime-start)
    (when (boundp 'rime-manual-lang) (makunbound 'rime-manual-lang))
    (if (member 'rime-chinese-mode rime-disable-predicates)
        (progn (setq rime-disable-predicates '(rime-english-mode)
                     rime-input-mode "English Mode")
               (flycheck-mode 1))
      (progn (setq rime-disable-predicates '(rime-chinese-mode)
                   rime-input-mode "中文模式")
             (flycheck-mode -1)))
    (if (string= current-input-method "rime")
        (message "%s" rime-input-mode)))

  (defun rime-change-state-manually ()
    "Change input mode temporarily, after file is modified, this staus is lost."
    (interactive)
    (rime-start)
    (setq rime-manual-lang
          (if (boundp 'rime-manual-lang)
              (not rime-manual-lang)
            (not (seq-find 'funcall rime-disable-predicates))))
    (if rime-manual-lang
        (message "临时英文")
      (message "临时中文"))))

(use-package cal-china-x
  :after calendar
  :config
  (setq calendar-mark-holidays-flag t
        cal-china-x-important-holidays cal-china-x-chinese-holidays
        calendar-holidays (append
                           cal-china-x-important-holidays
                           cal-china-x-general-holidays)))

(provide 'init-chinese)
