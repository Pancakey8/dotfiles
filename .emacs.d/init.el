;; use-package & melpa setup
; melpa
(require 'package)
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

; use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

; Ensure packages are installed
(require 'use-package)
(setq use-package-always-ensure t)

(setq use-package-verbose t)

;; Basic default settings
; Disable ugly UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

; Highlight parenthesis
(show-paren-mode 1)

; Highlight line
(global-hl-line-mode 1)

; Show relative lines
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

; Indentation
(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 80)

; Disable tilde files
(setq make-backup-files nil)

; Show column
(column-number-mode 1)

; Yes-no switch
(fset 'yes-or-no-p 'y-or-n-p)

; Font
(set-face-attribute 'default nil
		    :family "Iosevka"
		    :height 120)

; Leader key
(defvar pan-leader-key "C-.")

(defvar pan-leader-map (make-sparse-keymap)
  "Leader-mode map")

(global-set-key (kbd pan-leader-key) pan-leader-map)

;; Basic utilities
; God mode
(use-package god-mode
  :bind (("<escape>" . god-mode-all)
	 ("C-x C-1" . delete-other-windows)
	 ("C-x C-2" . split-window-below)
	 ("C-x C-3" . split-window-right)
	 ("C-x C-0" . delete-window)
	 :map god-local-mode-map
	 ("z" . repeat))
  :hook ((god-mode-enabled . my/god-mode-cursor)
         (god-mode-disabled . my/god-mode-cursor))
  :init (god-mode)
  :config
  (defun my/god-mode-cursor ()
    (if god-local-mode
        (progn
          (set-cursor-color (face-attribute 'warning :foreground)))
      (progn
        (set-cursor-color (face-attribute 'default :foreground)))))
  (setq god-exempt-major-modes nil)
  (setq god-exempt-predicates nil))

; Git integration
(use-package magit
  :commands (magit magit-status magit-init))

; Subword
(use-package syntax-subword
  :init (global-syntax-subword-mode))

; Command completion
(use-package vertico
  :init (vertico-mode))

; Drag mode
(use-package drag-stuff
  :init (drag-stuff-global-mode)
  :config
  (drag-stuff-define-keys))

; Better completion finding
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)))

; Marginalia on minibuffer
(use-package marginalia
  :init (marginalia-mode))

; Consult search
(use-package consult)

; File tree
(use-package treemacs
  :commands (treemacs)
  :bind (:map pan-leader-map
	      ("C-t" . treemacs)))

; Color theme
(use-package doom-themes
  :config
  (load-theme 'doom-opera t)
  (custom-set-faces
   '(default ((t (:background "#212121"))))))

; Company completion
(use-package company
  :hook (prog-mode . company-mode)
  :bind ("C-<tab>" . company-complete)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.1))

; Company maths symbols
(use-package company-math
  :after company
  :config
  (defun my/company-math-setup ()
    (setq-local company-backends
		(append '((company-math-symbols-latex company-latex-commands))
			company-backends))))

; Multicursor iedit
(use-package iedit)

; Org mode comments
(use-package poporg
  :bind (:map pan-leader-map
	      ("C-\"" . poporg-dwim)))
  
; Colored buffers (*compilation*)
(use-package xterm-color
  :hook (compilation-filter-hook . xterm-color-filter)
  :config
  (setq compilation-environment '("TERM=xterm-256color")))

; Which key
(use-package which-key
  :demand t
  :config (which-key-mode))

;; Languages setup
; Treesitter auto-installation
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

; Flycheck errors
(use-package flycheck
  :hook (after-init . global-flycheck-mode))

; Ada
(use-package ada-ts-mode
  :mode ("\\.ads\\'" "\\.adb\\'"))
(use-package gpr-ts-mode
  :mode ("\\.gpr\\'"))

; Rust
(use-package rust-ts-mode
  :mode ("\\.rs\\'"))

; Frama-C
(use-package acsl
  :load-path "~/.emacs.d/acsl/"
  :commands acsl-mode)

; Meson
(use-package meson-mode
  :mode ("\\(?:^\\|/\\)meson\\.build\\'")
  :hook (meson-mode . company-mode))

; Snippets
(use-package yasnippet
  :config
  (yas-global-mode 1))

; LSP
(use-package lsp-mode
  :commands lsp
  :hook ((ada-ts-mode
	      gpr-ts-mode
          rust-ts-mode
          c-ts-mode
          c++-ts-mode
          LaTeX-mode) . lsp)
  :bind (:map pan-leader-map
	      ("C-f" . lsp-format-buffer)
	      ("C-s C-d" . lsp-find-definition)
	      ("C-s C-i" . lsp-find-implementation)
	      ("C-s C-r" . lsp-find-references)
	      ("C-l C-r" . lsp-rename)
	      ("C-l C-a" . lsp-execute-code-action)
	      ("C-k" . lsp-ui-doc-glance))
  :config
  (add-to-list 'lsp-language-id-configuration '(acsl-mode . "c")))

(use-package lsp-ui
  :after lsp
  :hook (lsp . lsp-ui))

;; Document formats
; Org mode
(use-package org
  :mode (("\\.org\'" . org-mode))
  :config
  (plist-put org-format-latex-options :scale 1.5)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((gnuplot . t))))

(use-package gnuplot
  :after org
  (setq org-startup-with-inline-images t))

; Latex
(use-package auctex
  :hook (LaTeX-mode . company-mode)
  :bind (:map pan-leader-map
	      ("C-x C-a" . preview-document)
	      ("C-x C-A" . preview-clearout-document)
	      ("C-x C-h" . preview-at-point)
	      ("C-x C-H" . preview-clearout-at-point)
	      ("C-x C-v" . TeX-view)
	      ("C-x C-c" . my/latex-compile))
  :config
  (defun my/latex-compile () (interactive) (TeX-command "LaTeX" 'TeX-master-file -1)))

;; UI changes
; Modeline
(use-package moody
  :demand t

  :custom
  (x-underline-at-descent-line t)

  :config
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

; Dashboard
 (use-package dashboard
   :custom
   (dashboard-startup-banner 'logo)
   (dashboard-banner-logo-title "Emacs is ready")
   (initial-buffer-choice
    (lambda ()
      (get-buffer-create dashboard-buffer-name)))
   (dashboard-center-content t)
   (dashboard-show-shortcuts nil)
   (dashboard-vertically-center-content t)
   (dashboard-display-icons-p t)
   (dashboard-icon-type 'nerd-icons)
   (dashboard-set-heading-icons t)
   (dashboard-set-file-icons t)
   :config
   (dashboard-setup-startup-hook))

;; Custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   '(((output-dvi has-no-display-manager) "dvi2tty")
     ((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi")
     (output-pdf "Okular") (output-html "xdg-open")))
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#212121")))))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'magit-clean 'disabled nil)
