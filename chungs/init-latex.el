;; -*- lexical-binding: t; -*-

;; 外部安装 TeX-live
;; 安装 auctex
;; 设置
(setq ad-redefinition-action 'accept)
(use-package latex
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :bind
  (:map TeX-mode-map
        ("C-c C-m" . pdf-sync-forward-search))
  :init
  :config
  (setq LaTeX-electric-left-right-brace t)
  (add-hook 'LaTeX-mode-hook
            #'(lambda ()
                (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t))
                (setq-default TeX-engine 'xelatex)
                (setq
                 TeX-command-extra-options "-file-line-error -shell-escape"
                 TeX-command-default "XeLaTeX"
                 TeX-save-query  nil
                 TeX-electric-escape t
                 TeX-electric-math (cons "$" "$")
                 blink-matching-paren nil
                 org-latex-pdf-process
                 '(
                   "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                   "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                   "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                   "rm -fr %b.out %b.log %b.tex auto"
                   ))
                (setq org-latex-compiler "xelatex")
                (setq TeX-source-correlate-method 'synctex)
                (setq TeX-command-default "XeLaTeX")
                (setq  TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
                       TeX-view-program-selection '((output-pdf "PDF Tools"))
                       TeX-source-correlate-start-server t)
                (add-hook 'TeX-after-compilation-finished-functions
                          #'TeX-revert-document-buffer))))

(provide 'init-latex)
