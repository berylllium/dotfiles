;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; True in buffers created from terminal command "magit".
(setq berry/magit-terminal-buffer-p nil)


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;; (setq doom-font (font-spec :family "Fira Code" :size 13 :weight 'semi-light))
(setq doom-font (font-spec :family "Iosevka" :size 13 :weight 'regular))
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!
;;(setq doom-font (font-spec :family "JetBrainsMonoNL NFM" :size 12))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq projectile-project-search-path '(("~/prgm/" . 2)
                                       ("~/prgm/rust/fork/" . 1)
                                       ("~/prgm/godot/project/" . 2)))

(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; (plist-put org-latex-preview-appearance-options
;;            :page-width 0.8)

(add-hook 'org-mode-hook 'org-latex-preview-auto-mode)

;; (setq org-latex-preview-live t)
;; (setq org-latex-preview-live-debounce 0.25)

(turn-on-auto-fill)

;; Indentation
(setq-default indent-tabs-mode nil
              c-basic-offset 4
              tab-width 4)

;; Set fill column and enable global fill column indicator.
(setq-default fill-column 100)
(global-display-fill-column-indicator-mode)

;; Word wrapping.
(global-visual-line-mode t)

;; Disable evil-snipe.
(remove-hook 'doom-first-input-hook #'evil-snipe-mode)

;; Disable exit prompt when running in text-mode.
(if (not (display-graphic-p))
    (setq confirm-kill-emacs nil))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(after! evil-escape
  (setq evil-escape-key-sequence "jk"))

;; (after! rustic
;;   (setq rustic-format-trigger 'on-save))

(after! lsp-ui
  (setq lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-diagnostic-max-lines 8))

(after! doom-cli-env
  (add-to-list 'doom-env-allow "^SSH_"))

(map! :n "s" 'avy-goto-char-2)
(map! :n "S" 'avy-goto-line)

(map! :leader
      (:prefix ("e" . "eval")
       :desc "Eval last sexp"
       "l" 'eval-last-sexp
       :desc "Eval region"
       "r" 'eval-region
       :desc "Eval buffer"
       "b" 'eval-buffer)
      (:prefix "o"
               "o" (lambda () (interactive) (doom-project-find-file org-directory))))

(map! :leader
      :desc "M-x"
      "SPC" #'execute-extended-command)

(map! :after magit
      :map magit-mode-map
      :n "q" (lambda () (interactive) (if berry/magit-terminal-buffer-p
                                          (delete-frame)
                                        (quit-window))))
