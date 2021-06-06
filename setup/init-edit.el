;;; This file is aimed to make edit a little easier.

;;; hightlight part
;;; special thanks to seagle0128

;; Highlight matching parens

;; Highlight parenthesises
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

;; Highlight symbols
(use-package symbol-overlay
  :custom-face (symbol-overlay-default-face ((t (:inherit (region bold)))))
  :bind (("M-i" . symbol-overlay-put)
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)
         ("M-C" . symbol-overlay-remove-all)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook ((css-mode scss-mode less-css-mode html-mode web-mode php-mode) . rainbow-mode))

;; Highlight the current line
;; (use-package hl-line
  ;; :ensure nil
  ;; :hook
  ;; (after-init . global-hl-line-mode))

(use-package anzu
  :bind
  (([remap query-replace] . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp)
   ([remap isearch-query-replace] . anzu-isearch-query-replace)
   ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp)))


;; Automatic parenthesis pairing
;; (use-package elec-pair
;;   :ensure nil
;;   :hook (after-init . electric-pair-mode)
;;   :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(use-package highlight-indent-guides
  :config
  (setq  highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-responsive 'top)
  (setq highlight-indent-guides-delay 0))

;; Syntax checking
(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :init
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  :bind (:map prog-mode-map
         ("C-c TAB" . hs-toggle-hiding)
         ("M-+" . hs-show-all))
  :hook (prog-mode . hs-minor-mode))

(use-package diff-hl
  :config
  (global-diff-hl-mode))

(use-package avy
  :bind (("C-:" . avy-goto-char)
         ;; ("C-" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0))
  :hook (after-init . avy-setup-default)
  :config (setq avy-all-windows nil 
                avy-all-windows-alt t
                avy-background t
                avy-style 'pre))

(provide 'init-edit)
