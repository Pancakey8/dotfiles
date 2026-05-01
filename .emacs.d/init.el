(require 'package)
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-verbose t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

(setq make-backup-files nil)

(column-number-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)

(defvar pan-leader-map (make-sparse-keymap)
  "Pancake leader key")

(global-set-key (kbd "M-m") pan-leader-map)

(set-face-attribute 'default nil
		    :family "JetBrainsMono Nerd Font"
		    :height 120)

(setq org-agenda-files '("~/Documents/Agenda/"))

(use-package nerd-icons)

(use-package magit
  :commands (magit magit-status magit-init))

(use-package project
  :defer t
  :config
  (defun my/project-local-dir (dir)
    (cons 'transient dir))
  (add-hook 'project-find-functions #'my/project-local-dir 100))

(use-package syntax-subword
  :config
  (global-syntax-subword-mode))

(use-package vertico
  :demand t
  :config
  (vertico-mode)
  (vertico-buffer-mode))

(use-package drag-stuff
  :demand t
  :config
  (drag-stuff-global-mode)
  (drag-stuff-define-keys))

(use-package orderless
  :demand t
  :config
  (setq completion-styles '(orderless basic)))

(use-package marginalia
  :demand t
  :config
  (marginalia-mode))

(use-package consult
  :bind (("C-S-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)))

(use-package company
  :hook (prog-mode . company-mode)
  :bind ("C-<tab>" . company-complete)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.1))

(use-package xterm-color
  :hook (compilation-filter-hook . xterm-color-filter)
  :config
  (setq compilation-environment '("TERM=xterm-256color")))

(use-package which-key
  :demand t
  :config (which-key-mode))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c"   . mc/edit-lines)
         ("C->"           . mc/mark-next-like-this)
         ("C-<"           . mc/mark-previous-like-this)
         ("C-c m p"       . mc/mark-pop)
         ("C-c C-<"       . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click))
  :custom
  (mc/always-run-for-all t))

(use-package vterm
  :ensure t)

(setq major-mode-remap-alist
      '((c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (c-or-c++-mode . c-or-c++-ts-mode)))

(add-to-list 'auto-mode-alist '("\\.cppm\\'" . c++-ts-mode))

(use-package cmake-ts-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-ts-mode)
         ("\\.cmake\\'"         . cmake-ts-mode)))

(use-package haskell-mode
  :ensure t
  :mode ("\\.hs\\'" . haskell-mode)
  :hook (haskell-mode . interactive-haskell-mode))


(use-package eglot
  :hook ((c-ts-mode
          c++-ts-mode
          cmake-ts-mode
          haskell-mode) . eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 2000000)
  :bind (:map pan-leader-map
              ("k" . eldoc)
              ("l a" . eglot-code-actions)
              ("l r" . eglot-rename)
              ("f" . eglot-format-buffer)
              ("s i" . eglot-find-implementation)
              ("s d" . eglot-find-declaration)))

(use-package flymake
  :ensure nil
  :hook (eglot-managed-mode . flymake-mode)
  :bind (:map pan-leader-map
              ("d n" . flymake-goto-next-error)
              ("d p" . flymake-goto-prev-error)
              ("d l" . flymake-show-buffer-diagnostics))
  :config
  (setq flymake-fringe-indicator-position 'left-fringe))

(use-package dape
  :commands dape
  :custom
  (dape-buffer-window-arrangement 'gud))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-nord-light t)
  (doom-themes-org-config))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 40))

(use-package ace-window
  :bind (("M-o" . ace-window)))

(use-package emacs
  :bind (("C-x C-2" . split-window-below)
	 ("C-x C-3" . split-window-right)
	 ("C-x C-0" . delete-window)
	 ("C-x C-1" . delete-other-windows)
	 ("C-x C-o" . other-window)
	 ("C-x C-a" . maximize-window)
	 ("C-x C-M-a" . minimize-window)))

(use-package smartparens
  :init (smartparens-mode 1)
  :config
  (require 'smartparens-config))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("87fa3605a6501f9b90d337ed4d832213155e3a2e36a512984f83e847102a42f4"
     default))
 '(inhibit-startup-screen t)
 '(package-selected-packages nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
