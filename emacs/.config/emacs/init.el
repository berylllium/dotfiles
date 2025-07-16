;; Bootstraps.
;;; Bootstrap straight.
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

;; Emacs settings.
(use-package emacs
  :ensure nil
  :straight (:type built-in)
  :init
  ;; Disable base Emacs features.
  (setq inhibit-startup-message t
	make-backup-files nil)

  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (set-fringe-mode 10)
  (menu-bar-mode -1)
  ;; Set font.
  (set-face-attribute 'default nil :family "FiraCode Nerd Font Mono" :height 110)

  ;; Enable line numbering only in programming buffers.
  ;;(setq-default display-line-numbers-type 'relative)
  ;;(add-hook 'prog-mode-hook 'display-line-numbers-mode)
  
  ;; Disable the usage of tab characters.
  (setq indent-tabs-mode nil))

;; Functions.
(defun open-init-file ()
  "Open the init.el file in the current window."
  (interactive)
  (find-file user-init-file))
    
(defun switch-to-messages-buffer ()
  "Switch to the `*Messages*` buffer in the current window."
  (interactive)
  (with-current-buffer (messages-buffer)
    (goto-char (point-max))
    (switch-to-buffer (current-buffer))))

;; Packages.
;;; use-package configuration.
(setq use-package-always-ensure t) ;; Always ensure packages are installed.

;;; ========== GENERAL ==========
(use-package general
  :after evil
  :config
  (general-auto-unbind-keys)
  (general-evil-setup)

  (general-define-key
   :states '(normal insert motion visual emacs)
   :keymaps 'override
   :prefix-map 'tyrant-map
   :prefix "SPC"
   :non-normal-prefix "M-SPC")

  (general-create-definer tyrant-def :keymaps 'tyrant-map)
  (tyrant-def "" nil)

  (tyrant-def
    "SPC" '("M-x" . execute-extended-command)

    "b" (cons "buffer" (make-sparse-keymap))
    "bb" 'switch-to-buffer
    "bB" 'ibuffer
    "bk" 'kill-current-buffer
    "bm" 'switch-to-messages-buffer
    "bs" 'scratch-buffer
    "bl" '("last-buffer" . evil-switch-to-windows-last-buffer)

    "e" (cons "eval" (make-sparse-keymap))
    "ee" 'eval-expression
    "el" 'eval-last-sexp
    "er" 'eval-region

    "f" (cons "file" (make-sparse-keymap))
    "ff" 'find-file
    "fi" 'open-init-file
    "fj" 'dired-jump

    "h" (cons "help" (make-sparse-keymap))
    "hf" 'describe-function
    "hv" 'describe-variable

    "o" (cons "org" (make-sparse-keymap))
    "oo" '("org files" . (lambda () (interactive) (find-file org-directory)))))

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

(use-package avy
  :after evil
  :general
  (general-nmap
    "S" 'avy-goto-line
    "s" 'avy-goto-char-2))

;;; ========== Appearance ==========
;;; Gruvbox theme.
(use-package gruvbox-theme
  ;; Use a hook to only load the theme after the init hook as completed to ensure
  ;; the theme has been autoloaded.
  :hook (after-init-hook . (lambda () (load-theme 'gruvbox-dark-medium t))))

;;; Doom modeline.
(use-package doom-modeline
  :init
  (doom-modeline-mode 1))

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

;;; ========== Helpful Packages ==========
;;; Which key.
(use-package which-key
  :ensure nil ;; which-key is included in emacs v30.
  :straight (:type built-in)
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-echo-keystrokes 0.02
	which-key-idle-delay 0.4
	which-key-idle-secondary-delay 0.04
	which-key-sort-order 'which-key-key-order-alpha
	which-key-allow-evil-operators t))

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

;;; ========== Projectile ==========
(use-package projectile
  :general
  (tyrant-def
    "p" (cons "project" (make-sparse-keymap))
    "pf" 'projectile-find-file
    "pp" 'projectile-switch-project)
  :config
  (setq projectile-project-search-path '(("~/dotfiles" . 0) ("~/prgm" . 2)))
  (projectile-mode +1))

;;; ========== Coding ==========
;;; Automatic parens.
;;; NOTE: Only enabled in emacs-lisp-mode.
(use-package smartparens
  :hook ((prog-mode org-mode). smartparens-mode))

;;; LSP.
(use-package lsp-mode
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-headerline-breadcrumb-enable nil))

(use-package lsp-ui
  :commands lsp-ui-mode)

;; Tree sitter. HOPEFULLY THE BUILT IN ONE.
(defun berry/tree-sitter-install-languages ()
  "Installs all the languages in `treesit-language-source-alist'."
  (interactive)
  (mapc #'treesit-install-language-grammar
	(mapcar #'car treesit-language-source-alist)))

(use-package tree-sitter
  :ensure nil
  :straight (:type built-in)
  :init
  (setq treesit-language-source-alist
	`((bash . ("https://github.com/tree-sitter/tree-sitter-bash"
		   "v0.25.0"))
	  (elisp . ("https://github.com/Wilfred/tree-sitter-elisp"))
	  (json . ("https://github.com/tree-sitter/tree-sitter-json"
		   "v0.24.8"))
	  (make . ("https://github.com/alemuller/tree-sitter-make"))
	  (toml . ("https://github.com/ikatyang/tree-sitter-toml"
		   "v0.5.1"))
	  (rust . ("https://github.com/tree-sitter/tree-sitter-rust"
		   "v0.24.0")))))

(use-package rust-mode
  :hook (rust-mode . lsp)
  :init
  (setq rust-mode-treesitter-derive t
	rust-format-on-save t))

;;; ========== Git ==========
;;; Custom magit display buffer function.
(defun my-magit-display-buffer (buffer)
  "Display only the status buffer in the current window, otherwise create a new one."
  (display-buffer
   buffer (if (and (derived-mode-p 'magit-mode)
                   (memq (with-current-buffer buffer major-mode)
                         '(magit-process-mode
                           magit-revision-mode
                           magit-diff-mode
                           magit-stash-mode
                           magit-status-mode)))
              nil
            '(display-buffer-same-window))))

;;; Magit.
(use-package magit
  :general
  (tyrant-def
    "g" (cons "git" (make-sparse-keymap))
    "gg" 'magit-status)
  :config
  (setq magit-display-buffer-function #'my-magit-display-buffer))

;;; ========== Orgmode ==========
;;; Helper functions for org.
(defun org-init-appearance-h ()
  "Configure org UI."
  (setq org-indirect-buffer-display 'current-window
        org-hide-leading-stars t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t
        org-startup-indented t
        org-tags-column 0
        org-startup-folded nil)

  ;; Scale up the previews.
  (plist-put org-latex-preview-appearance-options :scale 1.2))

(defun org-init-hacks-h ()
  "Getting org to behave."
  ;; Open file links in current window, instead of new ones.
  (setf (alist-get 'file org-link-frame-setup) #'find-file)
  ;; Open directory links in dired.
  (add-to-list 'org-file-apps '(directory . emacs))
  (add-to-list 'org-file-apps '(remote . emacs)))

;;; We use a special fork of org that allows for async and automatic latex previews.
;;; TODO: Revert to emacs org when this fork gets merged.
(use-package org
  :ensure 
  :straight `(org
              :fork (:host nil
                     :repo "https://git.tecosaur.net/tec/org-mode.git"
                     :branch "dev"
                     :remote "tecosaur"
		     :depth 1)
              :files (:defaults "etc")
              :build t
              :pre-build
              (with-temp-file "org-version.el"
               (require 'lisp-mnt)
               (let ((version
                      (with-temp-buffer
                        (insert-file-contents "lisp/org.el")
                        (lm-header "version")))
                     (git-version
                      (string-trim
                       (with-temp-buffer
                         (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
                         (buffer-string)))))
                (insert
                 (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
                 (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
                 "(provide 'org-version)\n")))
              :pin nil)
  :hook ((org-mode . org-latex-preview-auto-mode)
         ;; Load all latex previews upon entering a buffer. This should
         ;; happen asyncronously.
         (org-mode . (lambda () (with-current-buffer (current-buffer)
                                  (org-latex-preview '(16))))))
  :init
  (setq org-directory "~/org")
  :config
  (org-init-appearance-h)
  (org-init-hacks-h)

  (setq org-latex-preview-live t
        org-latex-preview-live-debounce 0.25))
