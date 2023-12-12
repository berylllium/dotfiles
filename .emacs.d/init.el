;; Emacs will ask to follow symlinks by default. Disable this behavior.
(setq vc-follow-symlinks t)

;; Set the custom file so init.el doesn't get cluttered with auto-generated stuff.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Disable unwanted features
;; Start emacs in a scratch buffer
(setq inhibit-startup-message t)

;; Disable scrollbar
(scroll-bar-mode -1)

;; Disable toolbar
(tool-bar-mode -1)

;; Disable tooltips
(tooltip-mode -1)

;; Disable fringes
(set-fringe-mode 10)

;; Disable menubar
(menu-bar-mode -1)

;; Enable visual bell
;; The bell sound is very annoying and frequent, so replace it with a visual bell.
(setq visible-bell t)

;; System Settings
(setq berry/is-guix-system (eq system-type 'gnu/linux))

;; Package Management
;; Bootstrap straight.el.
(unless (featurep 'straight)
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

;; Use straight.el for use-package expressions.
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; General Configuration
;; Font
(set-face-attribute 'default nil :font "FiraCode Nerd Font Mono" :height 140)

;; Line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

;; Tabs
(setq-default tab-width 4
              indent-tabs-mode nil) ;; Always use spaces.

;; Theme
;; Use the gruvbox theme.
(use-package gruvbox-theme
  :init (load-theme 'gruvbox-dark-medium t))

;; Emacs completion
;; Ivy
(use-package ivy
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :init (ivy-mode 1))

;; Counsel
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

;; Which key
(use-package which-key
  :init (which-key-mode)
  :diminish 'which-key-mode
  :config
  (setq which-key-idle-delay 1))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Helpful
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; Keybindings
;; Evil
;; Use evil because I'm too used to vim keybindings.
(use-package undo-tree
  :config
  (setq undo-tree-auto-save-history nil)
  (global-undo-tree-mode 1))

(defun switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer nil))

(defun berry/evil-jk ()
  (interactive)
  (let* ((initial-key ?j)
         (final-key ?k)
         (timeout 0.5)
         (event (read-event nil nil timeout)))
    (if event
        ;; timeout met
        (if (and (characterp event) (= event final-key))
            (evil-normal-state)
          (insert initial-key)
          (push event unread-command-events))
      ;; timeout exceeded
      (insert initial-key))))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  
  (define-key evil-insert-state-map (kbd "j") 'berry/evil-jk)
  
  (define-key evil-normal-state-map (kbd "C-k") 'switch-to-last-buffer))

;; Evil collection
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Leader Bindings (general.el)
(use-package general
  :config
  (general-evil-setup t)
  
  (general-create-definer berry/leader-key-def
    :keymaps '(normal visual emacs)
    :prefix "\\"))

;; Org Mode
(defun berry/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1)
  (setq evil-auto-indent nil)
  
  ;; Preview all latex fragments.
  (setq current-prefix-arg '(16))
  (call-interactively 'org-latex-preview))

(use-package org
  :hook (org-mode . berry/org-mode-setup)
  :config
  
  (setq org-hide-emphasis-markers t)
  
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  
  (setq org-agenda-files '("~/documents/notes/informatica.org"))
  
  (define-key org-mode-map (kbd "<normal-state> C-k") 'switch-to-last-buffer)
  
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5)))

;; Fragtog
;; Package for toggling latex snippets in org files.
(use-package org-fragtog
  :after org
  :config
  (add-hook 'org-mode-hook 'org-fragtog-mode))

;; Org Appear
;; This package hides markup around words, and reveals them when the cursor is inside them.
(use-package org-appear
  :after org
  :config
  (add-hook 'org-mode-hook 'org-appear-mode))

;; Discord Rich Presence
(use-package elcord
  :straight (elcord :type git :host github :repo "berylllium/elcord")
  :after projectile
  :config
  (setq elcord-display-elapsed nil)
  (setq elcord-display-line-numbers nil)
  (setq elcord-display-project-name t)
  (setq elcord-use-major-mode-as-main-icon t)
  (setq elcord-idle-timer nil)
  (elcord-mode))

;; Development
;; Treemacs
(use-package treemacs
  :defer t
  :config
  (treemacs-hide-gitignored-files-mode nil)

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)

  (berry/leader-key-def
    "mm" 'treemacs-select-window
    "mc" 'treemacs))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

;; General Development Configs
;; Color Column (fill column, ruler)
(add-hook 'prog-mode-hook (lambda ()
                            (display-fill-column-indicator-mode)
                            (setq display-fill-column-indicator-column 120)))

;; Terminal
(defun berry/toggle-term ()
  "Toggle the terminal buffer window."
  (interactive)
  (let ((w (get-buffer-window "*terminal*")) (prgm "/bin/bash"))
    (if w
        (delete-window w) ;; Close terminal window.
      (progn ;; Open new terminal window.
        (setq w (split-window-below -20))
        (select-window w)
        (term prgm)))))

(defun berry/kill-toggle-term-buffer ()
  "Kill the terminal buffer."
  (interactive)
  (let ((w nil))
    (when (setq w (get-buffer "*terminal*"))
      (kill-buffer w))))

(define-key evil-normal-state-map (kbd "C-\\") 'berry/toggle-term)
(define-key evil-normal-state-map (kbd "C-|") 'berry/kill-toggle-term-buffer)

;; Git
;; Magit
(use-package magit
  :config
  (berry/leader-key-def
    "gg" 'magit-status))

;; Magit-todos
(use-package magit-todos)

;; Projectile
(use-package projectile
  :init
  (setq projectile-project-search-path '(("~/prgm/" . 2) ("~/prgm/godot/projects/" . 1) ("~/documents/tex/" . 2)
                                         ("~/documents/lise/" . 1)))
  :config
  (projectile-mode +1)
  
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  
  (berry/leader-key-def
    "pf" 'projectile-find-file
    "pp" 'projectile-switch-project
    "pc" 'projectile-compile-project
    "pr" 'projectile-run-project))

;; Treesitter
(use-package tree-sitter
  :hook ((c-mode c++-mode) . (lambda () (tree-sitter-mode) (tree-sitter-hl-mode)))
  :config
  (require 'tree-sitter))

(use-package tree-sitter-langs
  :config
  (require 'tree-sitter-langs))

;; Lsp
;; For some reason, the lsp lens feature lags my emacs out.
(use-package lsp-ui
  :config
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-doc-position 'bottom))

(use-package lsp-mode
  :hook 'lsp-enable-which-key-integration
  :commands lsp
  :config
  (setq lsp-lens-enable nil))

;; Flycheck
;; Flycheck replaces the old flymake.
(use-package flycheck
  :hook (lsp-mode . flycheck-mode))

;; C/C++
(use-package ccls
  :hook ((c-mode c++-mode) .
         (lambda () (require 'ccls) (lsp)))
  :config
  (setq ccls-initialization-options '(:compilationDatabaseDirectory "build/"))
  
  ;; Set style variables.
  (c-set-offset 'innamespace 0)
  
  (setq-default lsp-enable-indentation nil
                lsp-enable-on-type-formatting nil
                c-default-style "bsd"
                c-basic-offset 4
                tab-width 4
                gdb-show-main t))

;;(add-hook 'c++-mode-hook 'lsp)

;; Dap
;;(use-package dap-mode
;;  :hook ((c-mode c++-mode) . (lambda ()
;;                               (dap-mode 1)
;;                               (dap-ui-mode 1)
;;                               (dap-tooltip-mode 1)
;;                               (dap-ui-controls-mode 1)))
;;  :config
;;  (setq dap-auto-configure-features '(locals controls tooltip))
;;  (setq dap-auto-show-output nil))

;; C/C++
;;  (require 'dap-lldb)
;;
;;  (setq dap-lldb-debug-program '("/usr/bin/lldb-vscode"))
;;
;;  (setq dap-lldb-debugged-program-function (lambda ()
;;                                             (read-file-name "Select executable to debug: " (projectile-project-root))))
;;
;;  (dap-register-debug-template
;;   "LLDB Debug Launch"
;;   (list :type "lldb-vscode"
;;         :cwd nil
;;         :args nil
;;         :request "launch"
;;         :program nil))

;;(require 'dap-cpptools)

;;(defun berry/dap-cpptools-populate-start-args (conf)
;;  "Populate nil args with defaults."
;;  (lambda (conf)
;;    (dap--put-if-absent conf :program (list ((lambda ()
;;                                               (read-file-name "Select executable to debug: " (projectile-project-root))))))))
;;
;;(dap-register-debug-provider "cppdbg" 'berry/dap-cpptools-populate-start-args)
;;
;;(dap-register-debug-template
;; "Debug Launch"
;; (list :name "Debug Launch (VSCode)"
;;       :type "cppdbg"
;;       :cwd nil
;;       :args nil
;;       :request "launch"
;;       :program nil))

;;(berry/leader-key-def
;;  "dd" 'dap-debug
;;  "dl" 'dap-debug-last
;;  "db" 'dap-breakpoint-toggle
;;  "ds" 'dap-step-in
;;  "dn" 'dap-next
;;  "dc" 'dap-continue
;;  "dk" 'dap-disconnect)
