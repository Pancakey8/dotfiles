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

(setq inhibit-startup-screen t)

(show-paren-mode 1)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 80)

(setq make-backup-files nil)

(column-number-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)

(defvar pan-leader-map (make-sparse-keymap)
  "Pancake leader key")

(global-set-key (kbd "M-m") pan-leader-map)

(set-face-attribute 'default nil
		    :family "Iosevka Nerd Font Mono"
		    :height 120)

(setq org-agenda-files '("~/Documents/Agenda/"))

(use-package treemacs
  :commands (treemacs)
  :bind (:map pan-leader-map
	      ("t" . treemacs)))

(use-package magit
  :commands (magit magit-status magit-init))

(use-package project
  :defer t
  :config
  (defun my/project-local-dir (dir)
    (cons 'transient dir))
  (add-hook 'project-find-functions #'my/project-local-dir 100))

(use-package exec-path-from-shell
  :demand t
  :config
  (when (or (memq window-system '(mac ns x pgtk))
            (daemonp))
    (setq exec-path-from-shell-variables
          '("PATH"
            "MANPATH"
            "CAML_LD_LIBRARY_PATH"
            "OPAM_SWITCH_PREFIX"))
    (exec-path-from-shell-initialize)))

(use-package syntax-subword
  :config
  (global-syntax-subword-mode))

(use-package vertico
  :demand t
  :config
  (vertico-mode))

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

(use-package autothemer
  :ensure t)

(use-package tramp
  :defer t
  :config
  (tramp-enable-method "distrobox")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package pancake-theme
  :ensure nil
  :after autothemer
  :load-path "themes/"
  :init
  (add-to-list 'custom-theme-load-path
               (expand-file-name "themes/" user-emacs-directory))
  :config
  (load-theme 'pancake t))

(use-package xterm-color
  :hook (compilation-filter-hook . xterm-color-filter)
  :config
  (setq compilation-environment '("TERM=xterm-256color")))

(use-package which-key
  :demand t
  :config (which-key-mode))

(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c"   . mc/edit-lines)
         ("C->"           . mc/mark-next-like-this)
         ("C-<"           . mc/mark-previous-like-this)
         ("C-c m p"       . mc/mark-pop)
         ("C-c C-<"       . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click))
  :config
  (setq mc/always-run-for-all t))

(use-package vterm
  :ensure t)

(setq major-mode-remap-alist
      '((c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (c-or-c++-mode . c-or-c++-ts-mode)
        (csharp-mode . csharp-ts-mode)))

(add-to-list 'auto-mode-alist '("\\.cppm\\'" . c++-ts-mode))

(use-package cmake-ts-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-ts-mode)
         ("\\.cmake\\'"         . cmake-ts-mode)))

(use-package neocaml
  :ensure t
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((neocaml-mode neocaml-interface-mode) . ("ocamllsp")))))

(use-package utop
  :ensure t
  :after neocaml
  :hook (neocaml-mode . utop-minor-mode))

(use-package ocaml-eglot
  :ensure t
  :after neocaml
  :hook
  (neocaml-mode . ocaml-eglot)
  (ocaml-eglot . eglot-ensure))

(use-package haskell-mode
  :ensure t
  :mode ("\\.hs\\'" . haskell-mode)
  :hook (haskell-mode . interactive-haskell-mode))

(use-package eglot
  :hook ((c-ts-mode
          c++-ts-mode
          csharp-ts-mode
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

(use-package moody
  :demand t

  :custom
  (x-underline-at-descent-line t)

  :config
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("f063e60f5ed398a8bbe65fa71a7817b5b1936421b77ffe1720f39809d4936c16"
     "1170119313685ed8d504334ef9b4102cea173551d92ace51a6ef33ef2befb2f3"
     "85fb761925ef87dca08112168d4f37d6a8966aabdd90b9e7d8339f280ed5a491" default))
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
