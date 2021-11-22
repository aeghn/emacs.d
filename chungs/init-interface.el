;; -*- lexical-binding: t; -*-
(require 's)
(require 'dash)
(require 'minions)

(setq scroll-bar-mode nil)

(use-package minions
  :config (minions-mode 1))

(defun chin/shrink-path (full-path given-length)
  "Return fish-style truncated string based on FULL-PATH.
Optional parameter TRUNCATE-ALL will cause the function to truncate the last
directory too."
  (if (> (length full-path) given-length)
      (let* ((home (expand-file-name "~"))
             (path (replace-regexp-in-string
                    (s-concat "^" home) "~" full-path))
             (split (s-split "/" path 'omit-nulls))
             (split-len (length split))
             shrunk)
        (->> split
          (--map-indexed (if (= it-index (1- split-len))
                             it
                           (substring it 0 (if (s-starts-with? "." it) 2 1))))
          (s-join "/")
          (setq shrunk))
        (s-concat (unless (s-matches? (rx bos (or "~" "/")) shrunk) "/")
                  shrunk
                  (unless (s-ends-with? "/" shrunk) "/"))) full-path))

(defun buffer-status ()
  (cond (buffer-read-only (if (buffer-modified-p) "◇ ◠ ◇" "◇ ◡ ◇"))
        (overwrite-mode (if (buffer-modified-p) "□ ◠ □" "□ ◡ □"))
        (t  (if (buffer-modified-p) "○ ◠ ○" "○ ◡ ○"))))

;; Ⓐ Ⓑ Ⓒ Ⓓ Ⓔ Ⓕ Ⓖ Ⓗ Ⓘ Ⓙ Ⓚ Ⓛ Ⓜ Ⓝ Ⓞ Ⓟ Ⓠ Ⓡ Ⓢ Ⓣ Ⓤ Ⓥ Ⓦ Ⓧ Ⓨ Ⓩ
;; 🅐 🅑 🅒 🅓 🅔 🅕 🅖 🅗 🅘 🅙 🅚 🅛 🅜 🅝 🅞 🅟 🅠 🅡 🅢 🅣 🅤 🅥 🅦 🅧 🅨 🅩
;; ⓐ ⓑ ⓒ ⓓ ⓔ ⓕ ⓖ ⓗ ⓘ ⓙ ⓚ ⓛ ⓜ ⓝ ⓞ ⓟ ⓠ ⓡ ⓢ ⓣ ⓤ ⓥ ⓦ ⓧ ⓨ ⓩ

;; 🄰 🄱 🄲 🄳 🄴 🄵 🄶 🄷 🄸 🄹 🄺 🄻 🄼 🄽 🄾 🄿 🅀 🅁 🅂 🅃 🅄 🅅 🅆 🅇 🅈 🅉
;; 🅰 🅱 🅲 🅳 🅴 🅵 🅶 🅷 🅸 🅹 🅺 🅻 🅼 🅽 🅾 🅿 🆀 🆁 🆂 🆃 🆄 🆅 🆆 🆇 🆈 🆉

;; 🄋 ➀ ➁ ➂ ➃ ➄ ➅ ➆ ➇ ➈ ➉

(defun return-string-space (str)
  "if `str' is not blank return str and spaces"
  (if (not (or (equal str nil) (string-blank-p str)))
      (concat "    " str)
    ""))

(setq-default mode-line-format
              '("   "
                (:eval (propertize (if (buffer-file-name) (chin/shrink-path default-directory 15) "")))
                (:eval (propertize "%b" 'face '(:weight bold)))
                "      "
                (:eval (buffer-status))
                "      "
                minions-mode-line-modes
                (:eval (propertize (return-string-space vc-mode) 'face '()))
                (:eval (return-string-space (flycheck-mode-line-status-text)))
                "      "
                (:eval (propertize "%l:%C " 'face '(:weight bold)))
                (:eval (propertize "%P "))))


(global-display-line-numbers-mode 1) 

;;; Header
(setq-default frame-title-format '("%b — GNU Emacs" ))

(provide 'init-interface)
