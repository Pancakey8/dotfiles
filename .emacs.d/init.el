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

; Show column
(column-number-mode 1)

; Yes-no switch
(fset 'yes-or-no-p 'y-or-n-p)

; Font
(set-face-attribute 'default nil
		    :family "Iosevka Mono"
		    :height 120)

; Leader key
(defvar pan-leader-key "C-.")

(defvar pan-leader-map (make-sparse-keymap)
  "Leader-mode map")

(global-set-key (kbd pan-leader-key) pan-leader-map)

;; Basic utilities
; Git integration
(use-package magit
  :commands (magit magit-status magit-init))

; Command completion
(use-package vertico
  :init (vertico-mode))

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
	      ("t" . treemacs)))

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
	      ("\"" . poporg-dwim)))
  
; Colored buffers (*compilation*)
(use-package xterm-color
  :hook (compilation-filter-hook
	 .
	 (lambda ()
           (let ((inhibit-only-read t))
             (ansi-color-apply-on-region compilation-filter-start (point)))))
  :config
  (setq compilation-environment '("TERM=xterm-256color")))

; Which key
(use-package which-key
  :demand t
  :config (which-key-mode))

;; LSP mode setup
; Ada
(use-package ada-ts-mode
  :mode ("\\.ads\\'" "\\.adb\\'"))
(use-package gpr-ts-mode
  :mode ("\\.gpr\\'"))

; Snippets
(use-package yasnippet
  :config
  (yas-global-mode 1))

; LSP
(use-package lsp-mode
  :commands lsp
  :hook ((ada-ts-mode
	  gpr-ts-mode) . lsp)
  :bind (:map pan-leader-map
	      ("f" . lsp-format-buffer)
	      ("s d" . lsp-find-definition)
	      ("s i" . lsp-find-implementation)
	      ("s r" . lsp-find-references)
	      ("l r" . lsp-rename)
	      ("l a" . lsp-execute-code-action)
	      ("k" . lsp-ui-doc-glance)))

;; Document formats
; Org mode
(use-package org
  :mode (("\\.org\'" . org-mode))
  :config
  (plist-put org-format-latex-options :scale 1.5))

; Latex
(use-package auctex
  :hook (LaTeX-mode . company-mode)
  :bind (:map pan-leader-map
	      ("x a" . preview-document)
	      ("x A" . preview-clearout-document)
	      ("x h" . preview-at-point)
	      ("x H" . preview-clearout-at-point)
	      ("x v" . TeX-view)
	      ("x c" . my/latex-compile))
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
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#212121")))))
