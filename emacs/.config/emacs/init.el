;; Bootstrap straight.
(setq straight-use-package-by-default t)
(setq straight-vc-git-default-clone-depth 1)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Disable base emacs features.
(setq inhibit-startup-message t
      make-backup-files nil)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)

;; Packages.
;;; use-package configuration.
(setq use-package-always-ensure t) ;; Always ensure packages are installed.

;;; ========== EVIL ==========
;;; VIM bindings in emacs :).
(use-package evil
  :custom
  (evil-search-module 'evil-search)
  (evil-want-keybinding nil)
  (evil-want-C-u-scroll t)
  :config
  (evil-mode 1))

;;; Use jk to get out of insert mode.
(use-package evil-escape
  :config
  (setq-default evil-escape-key-sequence "jk")
  (evil-escape-mode))

;;; VIM vindings everywhere.
(use-package evil-collection
  :after evil
  :custom
  (evil-want-integration t)
  :config
  (evil-collection-init))

;;; ========== Appearance ==========
;;; Gruvbox theme.
(use-package gruvbox-theme
  ;; Use a hook to only load the theme after the init hook as completed to ensure
  ;; the theme has been autoloaded.
  :hook (after-init-hook . (lambda () (load-theme 'gruvbox-dark-medium t))))

;;; Highlighting todo's in comments. I got the keyword faces from the doom emacs config.
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :custom
  (hl-todo-highlight-punctuation ":")
  (hl-todo-keyword-faces
   '(;; For reminders to change or add something at a later date. TODO
     ("TODO" warning bold)
     ;; For code (or code paths) that are broken, unimplemented, or slow,
     ;; and may become bigger problems later.
     ("FIXME" error bold)
     ;; For code that needs to be revisited later, either to upstream it,
     ;; improve it, or address non-critical issues.
     ("REVIEW" font-lock-keyword-face bold)
     ;; For code smells where questionable practices are used
     ;; intentionally, and/or is likely to break in a future update.
     ("HACK" font-lock-constant-face bold)
     ;; For sections of code that just gotta go, and will be gone soon.
     ;; Specifically, this means the code is deprecated, not necessarily
     ;; the feature it enables.
     ("DEPRECATED" font-lock-doc-face bold)
     ;; Extra keywords commonly found in the wild, whose meaning may vary
     ;; from project to project.
     ("NOTE" success bold)
     ("BUG" error bold)
     ("XXX" font-lock-constant-face bold))))

;;; ========== Completion ========== 
;;; Completion.
(use-package corfu
  :after orderless
  :custom
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match t)
  (corfu-cycle t)
  :config
  (global-corfu-mode))

;;; Minibuffer completion.
(use-package vertico
  :custom
  (vertico-resize nil)
  (vertico-count 17)
  (vertico-cycle t)
  :config
  (vertico-mode))

;;; Orderless completion style.
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles orderless partial-completion))))
  (orderless-component-seperator #'orderless-escapable-split-on-space))

