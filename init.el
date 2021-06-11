(prefer-coding-system 'utf-8)

(defvar user-emacs-directory (file-truename "~/.emacs.d/"))

(defvar chin/configuration-files-directory (expand-file-name "chungs" user-emacs-directory)
  "Personal configuration directory")

(defvar chin/library-files-directory (expand-file-name "store" user-emacs-directory)
  "Personal library directory")

(defvar chin/temporary-files-directory (expand-file-name "litter" user-emacs-directory)
  "Personal temporary directory")
(unless (file-directory-p chin/temporary-files-directory) (mkdir chin/temporary-files-directory))

(unless (file-directory-p chin/temporary-files-directory) (mkdir chin/temporary-files-directory))

(setq custom-file (expand-file-name "custom.el" chin/temporary-files-directory))
(when (file-exists-p custom-file) (load custom-file :no-error :no-message))

(add-to-list 'load-path (expand-file-name chin/configuration-files-directory))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp"))
(add-to-list 'custom-theme-load-path (expand-file-name "themes" chin/library-files-directory))

(setq gc-cons-threshold (* 100 1024 1024))

(setq visible-bell nil)
(setq ring-bell-function 'ignore)

(require 'cl-lib)
(require 'init-basic)
(require 'init-default)
(require 'init-chinese)
(require 'init-completion)
(require 'init-latex)
(require 'init-ivy)
(require 'init-org)
(require 'init-window)
(require 'init-blog)
(require 'init-edit)
(require 'init-ui)
(require 'init-cc)
(require 'init-dired)
(require 'init-term)
(require 'jinp)
(require 'init-life)
(require 'init-interface)

(load-file "~/.emacs.d/site-lisp/cangjie.el")
