(use-package cc-mode
  :ensure nil
  :defines lsp-clients-clangd-executable lsp-clients-clangd-args
  :mode ("\\.cxx\\'" . cc-mode)
  :hook (c-mode . (lambda ()
                    (setq comment-start "// "
                          comment-end "")))
  :config
  (setq c-basic-offset 4)

  (defconst ccls-args nil)
  (defconst clangd-args '("-j=2"
                          "--background-index"
                          "--clang-tidy"
                          "--completion-style=bundled"
                          "--pch-storage=memory"
                          "--suggest-missing-includes"
                          "--header-insertion-decorators=0"))
  (with-eval-after-load 'lsp-mode
    ;; Prefer `clangd' over `ccls'
    (cond ((executable-find "clangd") (setq lsp-clients-clangd-executable "clangd"
                                            lsp-clients-clangd-args clangd-args))
          ((executable-find "ccls") (setq lsp-clients-clangd-executable "ccls"
                                          lsp-clients-clangd-args ccls-args))))
  :custom
  (c-offsets-alist '((inline-open           . 0)
                     (brace-list-open       . 0)
                     (inextern-lang         . 0)
                     (statement-case-open   . 4)
                     (statement-cont        . (c-lineup-ternary-bodies +))
                     (access-label          . -)
                     (case-label            . 0)
                     (member-init-intro     . +)
                     (topmost-intro         . 0)
                     (inlambda              . 0) ;; better indentation for lambda
                     (innamespace           . -) ;; no indentation after namespace
                     (arglist-cont-nonempty . +))))



(use-package clang-format
  :bind
  (:map c-mode-map ("C-c C-f" . clang-format-region)))

;; (use-package ccls
;; :hook ((c-mode c++-mode objc-mode) .
;; (lambda () (require 'ccls) (lsp))))


(provide 'init-cc)
