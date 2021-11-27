;;; stein-theme.el --- stein Theme

;; Copyright 2015-present, All rights reserved
;;
;; Code licensed under the MIT license

;; Credits:
;; https://github.com/dracula/emacs

;; author
;; Aeghn

;;; Code:
(deftheme stein)

(let*
    ((stein-bg      "#fbf8ef")
     (stein-current "#FFFFFF")
     (stein-fg      "#050505")
     (stein-comment "#595959")
     (stein-cyan    "#335EA8")
     (stein-green   "#006400")
     (stein-orange  "#FD8008")
     (stein-magenta "#CE2F1A")
     (stein-purple  "#990099")
     (stein-red     "#AA0000")
     (stein-yellow  "#C18001")
     (stein-blue    "#000088")
     ;; Other colors
     (bg0 "#BDBDBD")
     (bg2 "#D8D8D8")
     (bg3 "#FFFFFF")
     (fg0 "#171717")
     (fg2 "#393939")
     (fg3 "#4A4A4A")
     )
  (custom-theme-set-faces
   'stein
   `(cursor ((t (:background ,fg3))))
   `(default ((t (:background ,stein-bg :foreground ,stein-fg))))
   `(default-italic ((t (:slant italic))))
   `(ffap ((t (:foreground ,fg0))))
   `(fringe ((t (:background ,stein-bg :foreground ,fg0))))
   `(highlight ((t (:foreground ,stein-yellow :background ,bg3))))
   `(hl-line ((t (:background ,stein-current))))
   `(info-quoted-name ((t (:foreground ,stein-blue))))
   `(info-string ((t (:foreground ,stein-yellow))))
   `(lazy-highlight ((t (:foreground ,fg2 :background ,bg3))))
   `(link ((t (:foreground ,stein-cyan :underline t))))
   `(linum ((t (:slant italic :foreground ,bg0 :background ,stein-bg))))
   `(line-number ((t (:slant italic :foreground ,stein-comment :background ,stein-bg ))))
   `(minibuffer-prompt ((t (:weight bold :foreground ,stein-blue))))
   `(region ((t (:background ,stein-yellow :foreground ,stein-bg :extend t))))
   `(trailing-whitespace ((t (:foreground nil :background ,stein-blue))))
   `(vertical-border ((t (:foreground ,stein-comment))))

   `(success ((t (:foreground ,stein-green))))
   `(warning ((t (:foreground ,stein-blue))))
   `(error ((t (:foreground ,stein-red))))
   `(header-line ((t (:background ,stein-bg))))
   `(secondary-selection ((t (:background ,bg3))))
   ;; syntax
   `(font-lock-builtin-face ((t (:foreground ,stein-blue))))
   `(font-lock-comment-face ((t (:foreground ,stein-comment))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,stein-comment))))
   `(font-lock-constant-face ((t (:foreground ,stein-cyan))))
   `(font-lock-doc-face ((t (:foreground ,stein-comment))))
   `(font-lock-function-name-face ((t (:foreground ,stein-green :weight bold))))
   `(font-lock-keyword-face ((t (:weight bold :foreground ,stein-magenta))))
   `(font-lock-negation-char-face ((t (:foreground ,stein-cyan))))
   `(font-lock-preprocessor-face ((t (:foreground ,stein-blue))))
   `(font-lock-reference-face ((t (:foreground ,stein-cyan))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,stein-cyan))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,stein-purple))))
   `(font-lock-string-face ((t (:foreground ,stein-yellow))))
   `(font-lock-type-face ((t (:slant italic :foreground ,stein-purple))))
   `(font-lock-variable-name-face ((t (:foreground ,stein-fg
                                                   :weight bold))))
   `(font-lock-warning-face ((t (:foreground ,stein-blue :background ,bg2))))
   ;; auto-complete
   `(ac-completion-face ((t (:underline t :foreground ,stein-magenta))))
   ;; avy
   `(avy-lead-face ((t (:foreground "#ffffff" :background ,stein-red))))
   `(avy-lead-face-0 ((t (:foreground "#ffffff" :background ,stein-yellow))))
   `(avy-lead-face-1 ((t (:foreground "#ffffff" :background ,stein-blue))))
   `(avy-lead-face-2 ((t (:foreground "#ffffff" :background ,stein-purple))))
   ;; company
   `(company-echo-common ((t (:foreground ,stein-bg :background ,stein-fg))))
   `(company-preview ((t (:background ,stein-bg :foreground ,stein-orange))))
   `(company-preview-common ((t (:foreground ,bg2 :foreground ,fg3))))
   `(company-preview-search ((t (:foreground ,stein-purple :background ,stein-bg))))
   `(company-scrollbar-bg ((t (:background ,bg3))))
   `(company-scrollbar-fg ((t (:foreground ,stein-magenta))))
   `(company-template-field ((t (:inherit region))))
   `(company-tooltip ((t (:foreground ,fg2 :background ,stein-bg :weight bold))))
   `(company-tooltip-annotation ((t (:foreground ,stein-cyan))))
   `(company-tooltip-common ((t (:foreground ,fg3))))
   `(company-tooltip-common-selection ((t (:foreground ,stein-yellow))))
   `(company-tooltip-mouse ((t (:inherit highlight))))
   `(company-tooltip-selection ((t (:background ,bg3 :foreground ,fg3))))
   ;; diff-hl
   `(diff-hl-change ((t (:foreground ,stein-blue :background ,stein-blue))))
   `(diff-hl-delete ((t (:foreground ,stein-red :background ,stein-red))))
   `(diff-hl-insert ((t (:foreground ,stein-green :background ,stein-green))))
   ;; dired
   `(dired-directory ((t (:foreground ,stein-green :weight normal))))
   `(dired-flagged ((t (:foreground ,stein-magenta))))
   `(dired-header ((t (:foreground ,fg3 :background ,stein-bg))))
   `(dired-ignored ((t (:inherit shadow))))
   `(dired-mark ((t (:foreground ,stein-fg :weight bold))))
   `(dired-marked ((t (:foreground ,stein-blue :weight bold))))
   `(dired-perm-write ((t (:foreground ,fg3 :underline t))))
   `(dired-symlink ((t (:foreground ,stein-yellow :weight normal :slant italic))))
   `(dired-warning ((t (:foreground ,stein-blue :underline t))))
   `(diredp-compressed-file-name ((t (:foreground ,fg3))))
   `(diredp-compressed-file-suffix ((t (:foreground ,fg0))))
   `(diredp-date-time ((t (:foreground ,stein-fg))))
   `(diredp-deletion-file-name ((t (:foreground ,stein-magenta :background ,stein-current))))
   `(diredp-deletion ((t (:foreground ,stein-magenta :weight bold))))
   `(diredp-dir-heading ((t (:foreground ,fg2 :background ,bg0))))
   `(diredp-dir-name ((t (:inherit dired-directory))))
   `(diredp-dir-priv ((t (:inherit dired-directory))))
   `(diredp-executable-tag ((t (:foreground ,stein-blue))))
   `(diredp-file-name ((t (:foreground ,stein-fg))))
   `(diredp-file-suffix ((t (:foreground ,fg0))))
   `(diredp-flag-mark-line ((t (:foreground ,fg2 :slant italic :background ,stein-current))))
   `(diredp-flag-mark ((t (:foreground ,fg2 :weight bold :background ,stein-current))))
   `(diredp-ignored-file-name ((t (:foreground ,stein-fg))))
   `(diredp-mode-line-flagged ((t (:foreground ,stein-blue))))
   `(diredp-mode-line-marked ((t (:foreground ,stein-blue))))
   `(diredp-no-priv ((t (:foreground ,stein-fg))))
   `(diredp-number ((t (:foreground ,stein-cyan))))
   `(diredp-other-priv ((t (:foreground ,stein-blue))))
   `(diredp-rare-priv ((t (:foreground ,stein-blue))))
   `(diredp-read-priv ((t (:foreground ,stein-purple))))
   `(diredp-write-priv ((t (:foreground ,stein-magenta))))
   `(diredp-exec-priv ((t (:foreground ,stein-yellow))))
   `(diredp-symlink ((t (:foreground ,stein-blue))))
   `(diredp-link-priv ((t (:foreground ,stein-blue))))
   `(diredp-autofile-name ((t (:foreground ,stein-yellow))))
   `(diredp-tagged-autofile-name ((t (:foreground ,stein-yellow))))
   ;; enh-ruby
   `(enh-ruby-heredoc-delimiter-face ((t (:foreground ,stein-yellow))))
   `(enh-ruby-op-face ((t (:foreground ,stein-magenta))))
   `(enh-ruby-regexp-delimiter-face ((t (:foreground ,stein-yellow))))
   `(enh-ruby-string-delimiter-face ((t (:foreground ,stein-yellow))))
   ;; font-latex
   `(font-latex-bold-face ((t (:foreground ,stein-purple))))
   `(font-latex-italic-face ((t (:foreground ,stein-magenta :slant italic))))
   `(font-latex-match-reference-keywords ((t (:foreground ,stein-cyan))))
   `(font-latex-match-variable-keywords ((t (:foreground ,stein-fg))))
   `(font-latex-string-face ((t (:foreground ,stein-yellow))))
   ;; gnus-group
   `(gnus-group-mail-1 ((t (:foreground ,stein-magenta :weight bold))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-mail-1 :weight normal))))
   `(gnus-group-mail-2 ((t (:foreground ,stein-cyan :weight bold))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-mail-2 :weight normal))))
   `(gnus-group-mail-3 ((t (:foreground ,stein-comment :weight bold))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-mail-3 :weight normal))))
   `(gnus-group-mail-low ((t (:foreground ,stein-current :weight bold))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-mail-low :weight normal))))
   `(gnus-group-news-1 ((t (:foreground ,stein-magenta :weight bold))))
   `(gnus-group-news-1-empty ((t (:inherit gnus-group-news-1 :weight normal))))
   `(gnus-group-news-2 ((t (:foreground ,stein-cyan :weight bold))))
   `(gnus-group-news-2-empty ((t (:inherit gnus-group-news-2 :weight normal))))
   `(gnus-group-news-3 ((t (:foreground ,stein-comment :weight bold))))
   `(gnus-group-news-3-empty ((t (:inherit gnus-group-news-3 :weight normal))))
   `(gnus-group-news-4 ((t (:inherit gnus-group-news-low))))
   `(gnus-group-news-4-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-5 ((t (:inherit gnus-group-news-low))))
   `(gnus-group-news-5-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-6 ((t (:inherit gnus-group-news-low))))
   `(gnus-group-news-6-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-low ((t (:foreground ,stein-current :weight bold))))
   `(gnus-group-news-low-empty ((t (:inherit gnus-group-news-low :weight normal))))
   `(gnus-header-content ((t (:foreground ,stein-magenta))))
   `(gnus-header-from ((t (:foreground ,stein-fg))))
   `(gnus-header-name ((t (:foreground ,stein-purple))))
   `(gnus-header-subject ((t (:foreground ,stein-green :weight bold))))
   `(gnus-summary-markup-face ((t (:foreground ,stein-cyan))))
   `(gnus-summary-high-unread ((t (:foreground ,stein-magenta :weight bold))))
   `(gnus-summary-high-read ((t (:inherit gnus-summary-high-unread :weight normal))))
   `(gnus-summary-high-ancient ((t (:inherit gnus-summary-high-read))))
   `(gnus-summary-high-ticked ((t (:inherit gnus-summary-high-read :underline t))))
   `(gnus-summary-normal-unread ((t (:foreground ,stein-orange :weight bold))))
   `(gnus-summary-normal-read ((t (:inherit gnus-summary-normal-unread :weight normal))))
   `(gnus-summary-normal-ancient ((t (:inherit gnus-summary-normal-read))))
   `(gnus-summary-normal-ticked ((t (:inherit gnus-summary-normal-read :underline t))))
   `(gnus-summary-low-unread ((t (:foreground ,stein-comment :weight bold))))
   `(gnus-summary-low-read ((t (:inherit gnus-summary-low-unread :weight normal))))
   `(gnus-summary-low-ancient ((t (:inherit gnus-summary-low-read))))
   `(gnus-summary-low-ticked ((t (:inherit gnus-summary-low-read :underline t))))
   `(gnus-summary-selected ((t (:inverse-video t))))
   ;; haskell-mode
   `(haskell-operator-face ((t (:foreground ,stein-magenta))))
   `(haskell-constructor-face ((t (:foreground ,stein-purple))))
   ;; helm
   `(helm-bookmark-w3m ((t (:foreground ,stein-purple))))
   `(helm-buffer-not-saved ((t (:foreground ,stein-purple :background ,stein-bg))))
   `(helm-buffer-process ((t (:foreground ,stein-blue :background ,stein-bg))))
   `(helm-buffer-saved-out ((t (:foreground ,stein-fg :background ,stein-bg))))
   `(helm-buffer-size ((t (:foreground ,stein-fg :background ,stein-bg))))
   `(helm-candidate-number ((t (:foreground ,stein-bg :background ,stein-fg))))
   `(helm-ff-directory ((t (:foreground ,stein-green :background ,stein-bg :weight bold))))
   `(helm-ff-executable ((t (:foreground ,stein-orange :background ,stein-bg :weight normal))))
   `(helm-ff-file ((t (:foreground ,stein-fg :background ,stein-bg :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,stein-magenta :background ,stein-bg :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,stein-bg :background ,stein-magenta :weight normal))))
   `(helm-ff-symlink ((t (:foreground ,stein-magenta :background ,stein-bg :weight bold))))
   `(helm-grep-cmd-line ((t (:foreground ,stein-fg :background ,stein-bg))))
   `(helm-grep-file ((t (:foreground ,stein-fg :background ,stein-bg))))
   `(helm-grep-finish ((t (:foreground ,fg2 :background ,stein-bg))))
   `(helm-grep-lineno ((t (:foreground ,stein-fg :background ,stein-bg))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((t (:foreground ,stein-green :background ,stein-bg))))
   `(helm-header ((t (:foreground ,fg2 :background ,stein-bg :underline nil :box nil))))
   `(helm-moccur-buffer ((t (:foreground ,stein-green :background ,stein-bg))))
   `(helm-selection ((t (:background ,bg2 :underline nil))))
   `(helm-selection-line ((t (:background ,bg2))))
   `(helm-separator ((t (:foreground ,stein-purple :background ,stein-bg))))
   `(helm-source-go-package-godoc-description ((t (:foreground ,stein-yellow))))
   `(helm-source-header ((t (:foreground ,stein-magenta :background ,stein-bg :underline nil :weight bold))))
   `(helm-time-zone-current ((t (:foreground ,stein-blue :background ,stein-bg))))
   `(helm-time-zone-home ((t (:foreground ,stein-purple :background ,stein-bg))))
   `(helm-visible-mark ((t (:foreground ,stein-bg :background ,bg3))))
   ;; highlight-indentation minor mode
   `(highlight-indentation-face ((t (:background ,bg2))))
   ;; icicle
   `(icicle-whitespace-highlight ((t (:background ,stein-fg))))
   `(icicle-special-candidate ((t (:foreground ,fg2))))
   `(icicle-extra-candidate ((t (:foreground ,fg2))))
   `(icicle-search-main-regexp-others ((t (:foreground ,stein-fg))))
   `(icicle-search-current-input ((t (:foreground ,stein-magenta))))
   `(icicle-search-context-level-8 ((t (:foreground ,stein-blue))))
   `(icicle-search-context-level-7 ((t (:foreground ,stein-blue))))
   `(icicle-search-context-level-6 ((t (:foreground ,stein-blue))))
   `(icicle-search-context-level-5 ((t (:foreground ,stein-blue))))
   `(icicle-search-context-level-4 ((t (:foreground ,stein-blue))))
   `(icicle-search-context-level-3 ((t (:foreground ,stein-blue))))
   `(icicle-search-context-level-2 ((t (:foreground ,stein-blue))))
   `(icicle-search-context-level-1 ((t (:foreground ,stein-blue))))
   `(icicle-search-main-regexp-current ((t (:foreground ,stein-fg))))
   `(icicle-saved-candidate ((t (:foreground ,stein-fg))))
   `(icicle-proxy-candidate ((t (:foreground ,stein-fg))))
   `(icicle-mustmatch-completion ((t (:foreground ,stein-purple))))
   `(icicle-multi-command-completion ((t (:foreground ,fg2 :background ,bg2))))
   `(icicle-msg-emphasis ((t (:foreground ,stein-green))))
   `(icicle-mode-line-help ((t (:foreground ,fg0))))
   `(icicle-match-highlight-minibuffer ((t (:foreground ,stein-blue))))
   `(icicle-match-highlight-Completions ((t (:foreground ,stein-green))))
   `(icicle-key-complete-menu-local ((t (:foreground ,stein-fg))))
   `(icicle-key-complete-menu ((t (:foreground ,stein-fg))))
   `(icicle-input-completion-fail-lax ((t (:foreground ,stein-magenta))))
   `(icicle-input-completion-fail ((t (:foreground ,stein-magenta))))
   `(icicle-historical-candidate-other ((t (:foreground ,stein-fg))))
   `(icicle-historical-candidate ((t (:foreground ,stein-fg))))
   `(icicle-current-candidate-highlight ((t (:foreground ,stein-blue :background ,bg3))))
   `(icicle-Completions-instruction-2 ((t (:foreground ,fg0))))
   `(icicle-Completions-instruction-1 ((t (:foreground ,fg0))))
   `(icicle-completion ((t (:foreground ,stein-fg))))
   `(icicle-complete-input ((t (:foreground ,stein-blue))))
   `(icicle-common-match-highlight-Completions ((t (:foreground ,stein-purple))))
   `(icicle-candidate-part ((t (:foreground ,stein-fg))))
   `(icicle-annotation ((t (:foreground ,fg0))))
   ;; icomplete
   `(icompletep-determined ((t (:foreground ,stein-blue))))
   ;; ido
   `(ido-first-match ((t (:foreground ,stein-magenta :weight bold))))
   `(ido-only-match ((t (:foreground ,stein-blue))))
   `(ido-subdir ((t (:foreground ,stein-blue))))
   `(ido-virtual ((t (:foreground ,stein-cyan))))
   `(ido-incomplete-regexp ((t (:inherit font-lock-warning-face))))
   `(ido-indicator ((t (:foreground ,stein-fg :background ,stein-magenta))))
   ;; isearch
   `(isearch ((t (:weight bold :foreground ,stein-blue :background ,bg3))))
   `(isearch-fail ((t (:foreground ,stein-bg :background ,stein-blue))))
   ;; jde-java
   `(jde-java-font-lock-constant-face ((t (:foreground ,stein-cyan))))
   `(jde-java-font-lock-modifier-face ((t (:foreground ,stein-magenta))))
   `(jde-java-font-lock-number-face ((t (:foreground ,stein-fg))))
   `(jde-java-font-lock-package-face ((t (:foreground ,stein-fg))))
   `(jde-java-font-lock-private-face ((t (:foreground ,stein-magenta))))
   `(jde-java-font-lock-public-face ((t (:foreground ,stein-magenta))))
   ;; js2-mode
   `(js2-external-variable ((t (:foreground ,stein-purple))))
   `(js2-function-param ((t (:foreground ,stein-cyan))))
   `(js2-jsdoc-html-tag-delimiter ((t (:foreground ,stein-yellow))))
   `(js2-jsdoc-html-tag-name ((t (:foreground ,stein-orange))))
   `(js2-jsdoc-value ((t (:foreground ,stein-yellow))))
   `(js2-private-function-call ((t (:foreground ,stein-cyan))))
   `(js2-private-member ((t (:foreground ,fg3))))
   ;; js3-mode
   `(js3-error-face ((t (:underline ,stein-blue))))
   `(js3-external-variable-face ((t (:foreground ,stein-fg))))
   `(js3-function-param-face ((t (:foreground ,stein-magenta))))
   `(js3-instance-member-face ((t (:foreground ,stein-cyan))))
   `(js3-jsdoc-tag-face ((t (:foreground ,stein-magenta))))
   `(js3-warning-face ((t (:underline ,stein-magenta))))
   ;; magit
   `(magit-branch-local ((t (:foreground ,stein-cyan))))
   `(magit-branch-remote ((t (:foreground ,stein-green))))
   `(magit-tag ((t (:foreground ,stein-blue))))
   `(magit-section-heading ((t (:foreground ,stein-magenta :weight bold))))
   `(magit-section-highlight ((t (:background ,bg3 :extend t))))
   `(magit-diff-context-highlight ((t (:background ,bg3
                                                   :foreground ,fg3
                                                   :extend t))))
   `(magit-diff-revision-summary ((t (:foreground ,stein-blue
                                                  :background ,stein-bg
                                                  :weight bold))))
   `(magit-diff-revision-summary-highlight ((t (:foreground ,stein-blue
                                                            :background ,bg3
                                                            :weight bold
                                                            :extend t))))
   ;; the four following lines are just a patch of the
   ;; upstream color to add the extend keyword.
   `(magit-diff-added ((t (:background "#335533"
                                       :foreground "#ddffdd"
                                       :extend t))))
   `(magit-diff-added-highlight ((t (:background "#336633"
                                                 :foreground "#cceecc"
                                                 :extend t))))
   `(magit-diff-removed ((t (:background "#553333"
                                         :foreground "#ffdddd"
                                         :extend t))))
   `(magit-diff-removed-highlight ((t (:background "#663333"
                                                   :foreground "#eecccc"
                                                   :extend t))))
   `(magit-diff-file-heading ((t (:foreground ,stein-fg))))
   `(magit-diff-file-heading-highlight ((t (:inherit magit-section-highlight))))
   `(magit-diffstat-added ((t (:foreground ,stein-green))))
   `(magit-diffstat-removed ((t (:foreground ,stein-red))))
   `(magit-hash ((t (:foreground ,fg2))))
   `(magit-hunk-heading ((t (:background ,bg3))))
   `(magit-hunk-heading-highlight ((t (:background ,bg3))))
   `(magit-item-highlight ((t (:background ,bg3))))
   `(magit-log-author ((t (:foreground ,fg3))))
   `(magit-process-ng ((t (:foreground ,stein-blue :weight bold))))
   `(magit-process-ok ((t (:foreground ,stein-green :weight bold))))
   ;; message
   `(message-mml ((t (:foreground ,stein-green :weight normal))))
   `(message-header-xheader ((t (:foreground ,stein-cyan :weight normal))))
   ;; mini-modeline
   `(mini-modeline-mode-line ((t (:background ,stein-yellow :box nil))))
   `(mini-modeline-mode-line-inactive ((t (:background ,stein-comment :box nil))))
   ;; mode-line
   `(mode-line ((t (:foreground nil :background nil :overline ,bg0))))
   `(mode-line-inactive ((t (:foreground ,fg0 :overline ,bg2))))
   `(mode-line-buffer-id ((t (:foreground ,stein-blue :weight bold))))
   `(mode-line-position-face ((t (:foreground ,stein-purple))))
   ;; mu4e
   `(mu4e-unread-face ((t (:foreground ,stein-magenta :weight normal))))
   `(mu4e-view-url-number-face ((t (:foreground ,stein-purple))))
   `(mu4e-highlight-face ((t (:background ,stein-bg
                                          :foreground ,stein-yellow
                                          :extend t))))
   `(mu4e-header-highlight-face ((t (:background ,stein-current
                                                 :foreground ,stein-fg
                                                 :underline nil :weight bold
                                                 :extend t))))
   `(mu4e-header-key-face ((t (:inherit message-mml))))
   `(mu4e-header-marks-face ((t (:foreground ,stein-purple))))
   `(mu4e-cited-1-face ((t (:foreground ,stein-purple))))
   `(mu4e-cited-2-face ((t (:foreground ,stein-blue))))
   `(mu4e-cited-3-face ((t (:foreground ,stein-comment))))
   `(mu4e-cited-4-face ((t (:foreground ,fg2))))
   `(mu4e-cited-5-face ((t (:foreground ,fg3))))
   ;; org
   `(org-agenda-date ((t (:foreground ,stein-cyan :underline nil))))
   `(org-agenda-dimmed-todo-face ((t (:foreground ,stein-comment))))
   `(org-agenda-done ((t (:foreground ,stein-green))))
   `(org-agenda-structure ((t (:foreground ,stein-purple))))
   `(org-block ((t (:foreground ,stein-blue))))
   `(org-code ((t (:foreground ,stein-yellow))))
   `(org-column ((t (:background ,bg0))))
   `(org-column-title ((t (:inherit org-column :weight bold :underline t))))
   `(org-date ((t (:foreground ,stein-cyan :underline t))))
   `(org-document-info ((t (:foreground ,stein-orange))))
   `(org-document-info-keyword ((t (:foreground ,stein-comment))))
   `(org-document-title ((t (:weight bold :foreground ,stein-blue :height 1.44))))
   `(org-done ((t (:foreground ,stein-comment :weight normal :strike-through t))))
   `(org-ellipsis ((t (:foreground ,stein-comment))))
   `(org-footnote ((t (:foreground ,stein-orange))))
   `(org-formula ((t (:foreground ,stein-magenta))))
   `(org-headline-done ((t (:foreground ,stein-comment :weight normal :strike-through t))))
   `(org-hide ((t (:foreground ,stein-bg :background ,stein-bg))))
   `(org-level-1 ((t (:inherit bold :foreground ,stein-green ))))
   `(org-level-2 ((t (:inherit bold :foreground ,stein-cyan ))))
   `(org-level-3 ((t (:weight normal :foreground ,stein-orange ))))
   `(org-level-4 ((t (:weight normal :foreground ,stein-blue))))
   `(org-level-5 ((t (:weight normal :foreground ,stein-magenta))))
   `(org-level-6 ((t (:weight normal :foreground ,stein-purple))))
   `(org-level-7 ((t (:weight normal :foreground ,stein-green))))
   `(org-level-8 ((t (:weight normal :foreground ,stein-blue))))
   `(org-link ((t (:foreground ,stein-cyan :underline t))))
   `(org-priority ((t (:foreground ,stein-cyan))))
   `(org-scheduled ((t (:foreground ,stein-green))))
   `(org-scheduled-previously ((t (:foreground ,stein-yellow))))
   `(org-scheduled-today ((t (:foreground ,stein-green))))
   `(org-sexp-date ((t (:foreground ,fg0))))
   `(org-special-keyword ((t (:foreground ,stein-yellow))))
   `(org-table ((t (:foreground ,stein-purple))))
   `(org-tag ((t (:foreground ,stein-magenta :weight bold :background ,bg2))))
   `(org-upcoming-deadline ((t (:foreground ,stein-yellow))))
   `(org-warning ((t (:weight bold :foreground ,stein-magenta))))
   ;; outline
   `(outline-1 ((t (:foreground ,stein-green))))
   `(outline-2 ((t (:foreground ,stein-purple))))
   `(outline-3 ((t (:foreground ,stein-cyan))))
   `(outline-4 ((t (:foreground ,stein-yellow))))
   `(outline-5 ((t (:foreground ,stein-blue))))
   `(outline-6 ((t (:foreground ,stein-orange))))
   ;; powerline
   `(powerline-evil-base-face ((t (:foreground ,bg2))))
   `(powerline-evil-emacs-face ((t (:inherit powerline-evil-base-face :background ,stein-yellow))))
   `(powerline-evil-insert-face ((t (:inherit powerline-evil-base-face :background ,stein-cyan))))
   `(powerline-evil-motion-face ((t (:inherit powerline-evil-base-face :background ,stein-purple))))
   `(powerline-evil-normal-face ((t (:inherit powerline-evil-base-face :background ,stein-green))))
   `(powerline-evil-operator-face ((t (:inherit powerline-evil-base-face :background ,stein-magenta))))
   `(powerline-evil-replace-face ((t (:inherit powerline-evil-base-face :background ,stein-red))))
   `(powerline-evil-visual-face ((t (:inherit powerline-evil-base-face :background ,stein-blue))))
   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,stein-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,stein-cyan))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,stein-purple))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,stein-magenta))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,stein-blue))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,stein-green))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,stein-yellow))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,stein-orange))))
   `(rainbow-delimiters-unmatched-face ((t (:foreground ,stein-blue))))
   ;; rpm-spec
   `(rpm-spec-dir-face ((t (:foreground ,stein-green))))
   `(rpm-spec-doc-face ((t (:foreground ,stein-magenta))))
   `(rpm-spec-ghost-face ((t (:foreground ,stein-purple))))
   `(rpm-spec-macro-face ((t (:foreground ,stein-yellow))))
   `(rpm-spec-obsolete-tag-face ((t (:inherit font-lock-warning-face))))
   `(rpm-spec-package-face ((t (:foreground ,stein-purple))))
   `(rpm-spec-section-face ((t (:foreground ,stein-yellow))))
   `(rpm-spec-tag-face ((t (:foreground ,stein-cyan))))
   `(rpm-spec-var-face ((t (:foreground ,stein-blue))))
   ;; shell-mode
   `(sh-heredoc ((t (:foreground ,stein-cyan))))
   `(sh-quoted-exec ((t (:foreground ,stein-green))))
   ;; show-paren
   `(show-paren-match-face ((t (:background unspecified
                                            :foreground ,stein-cyan
                                            :weight bold))))
   `(show-paren-match ((t (:background ,stein-yellow
                                       :foreground ,stein-blue
                                       :weight bold))))
   `(show-paren-match-expression ((t (:inherit region))))
   `(show-paren-mismatch ((t (:inherit font-lock-warning-face))))
   ;; slime
   `(slime-repl-inputed-output-face ((t (:foreground ,stein-purple))))
   ;; spam
   `(spam ((t (:inherit gnus-summary-normal-read :foreground ,stein-blue
                        :strike-through t :slant oblique))))
   ;; tab-bar & tab-line (since Emacs 27.1))))
   `(tab-bar ((t (:foreground ,stein-magenta :background ,bg2
                              :inherit variable-pitch))))
   `(tab-bar-tab ((t (:background ,stein-current :inherit tab-bar))))
   `(tab-bar-tab-inactive ((t (:foreground ,stein-purple :background ,bg3
                                           :inherit tab-bar-tab))))
   `(tab-line ((t (:height 0.9 :foreground ,stein-magenta
                           :background ,bg2 :inherit variable-pitch))))
   `(tab-line-tab ((t (:background ,stein-current :inherit tab-line))))
   `(tab-line-tab-inactive ((t (:foreground ,stein-purple :background ,bg3
                                            :inherit tab-line-tab))))
   ;; term
   `(term ((t (:foreground ,stein-fg :background ,stein-bg))))
   `(term-color-black ((t (:foreground ,stein-bg :background ,stein-bg))))
   `(term-color-blue ((t (:foreground ,stein-purple :background ,stein-purple))))
   `(term-color-cyan ((t (:foreground ,stein-cyan :background ,stein-cyan))))
   `(term-color-green ((t (:foreground ,stein-green :background ,stein-green))))
   `(term-color-magenta ((t (:foreground ,stein-magenta :background ,stein-magenta))))
   `(term-color-red ((t (:foreground ,stein-red :background ,stein-red))))
   `(term-color-white ((t (:foreground ,stein-fg :background ,stein-fg))))
   `(term-color-yellow ((t (:foreground ,stein-yellow :background ,stein-yellow))))
   ;; undo-tree
   `(undo-tree-visualizer-current-face ((t (:foreground ,stein-blue))))
   `(undo-tree-visualizer-default-face ((t (:foreground ,fg2))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,stein-purple))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,stein-fg))))
   ;; web-mode
   `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))
   `(web-mode-doctype-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-function-name-face ((t (:inherit ,font-lock-function-name-face))))
   `(web-mode-html-attr-name-face ((t (:foreground ,stein-purple))))
   `(web-mode-html-attr-value-face ((t (:foreground ,stein-green))))
   `(web-mode-html-tag-face ((t (:foreground ,stein-magenta :weight bold))))
   `(web-mode-keyword-face ((t (:foreground ,stein-magenta))))
   `(web-mode-string-face ((t (:foreground ,stein-yellow))))
   `(web-mode-type-face ((t (:inherit ,font-lock-type-face))))
   `(web-mode-warning-face ((t (:inherit ,font-lock-warning-face))))
   ;; which-func
   `(which-func ((t (:inherit ,font-lock-function-name-face))))
   ;; whitespace
   `(whitespace-big-indent ((t (:background ,stein-red :foreground ,stein-red))))
   `(whitespace-empty ((t (:background ,stein-blue :foreground ,stein-red))))
   `(whitespace-hspace ((t (:background ,bg3 :foreground ,stein-comment))))
   `(whitespace-indentation ((t (:background ,stein-blue :foreground ,stein-red))))
   `(whitespace-line ((t (:background ,stein-bg :foreground ,stein-magenta))))
   `(whitespace-newline ((t (:foreground ,stein-comment))))
   `(whitespace-space ((t (:background ,stein-bg :foreground ,stein-comment))))
   `(whitespace-space-after-tab ((t (:background ,stein-blue :foreground ,stein-red))))
   `(whitespace-space-before-tab ((t (:background ,stein-blue :foreground ,stein-red))))
   `(whitespace-tab ((t (:background ,bg2 :foreground ,stein-comment))))
   `(whitespace-trailing ((t (:inherit trailing-whitespace))))
   ;; yard-mode
   `(yard-tag-face ((t (:inherit ,font-lock-builtin-face))))
   `(yard-directive-face ((t (:inherit ,font-lock-builtin-face))))
   )

  (custom-theme-set-variables
   'stein
   `(pdf-view-midnight-colors '(,stein-fg . ,stein-bg))
   `(org-todo-keyword-faces
     '(("CANCELLED" :foreground ,stein-comment :weight bold)
       ("TODO" :weight bold :foreground ,stein-blue )
       ("STARTED" :weight bold :foreground ,stein-green )
       ("WAIT" :weight bold :foreground ,stein-cyan )
       ("DONE" :weight bold :foreground ,stein-comment )

       ))
   `(mouse-scroll-delay 0)
   `(mouse-wheel-flip-direction t)
   `(mouse-wheel-progressive-speed nil)
   `(mouse-wheel-scroll-amount '(0.03))
   `(mouse-wheel-tilt-scroll t)))

(provide-theme 'stein)

;;; stein-theme.el ends here
