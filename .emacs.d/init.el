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

(setq select-enable-clipboard nil)

(use-package evil
  :init
  (setq evil-want-C-u-scroll t
        evil-want-keybinding nil
        evil-want-integration t)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode 1))

(use-package tree-sitter
  :hook (prog-mode . tree-sitter-mode)
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :after tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

  (add-to-list 'major-mode-remap-alist '(ada-mode . ada-ts-mode))
  (add-to-list 'major-mode-remap-alist '(gpr-ts-mode . ada-ts-mode))

  (add-to-list 'tree-sitter-major-mode-language-alist '(ada-ts-mode . ada))
  (add-to-list 'tree-sitter-major-mode-language-alist '(gpr-ts-mode . gpr))

  (defun my/tree-sitter-ensure-language ()
    (unless (tree-sitter-language-available-p major-mode)
      (ignore-errors
        (tree-sitter-langs-install-grammars)
        (message "Installed missing Tree-sitter grammar for %s" major-mode))))
  (add-hook 'tree-sitter-after-on-hook #'my/tree-sitter-ensure-language))

(add-to-list 'auto-mode-alist '("\\.c\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))

(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package kotlin-mode
  :mode ("\\.kt\\'"))

(use-package ada-ts-mode
  :mode ("\\.adb\\'" "\\.ads\\'")
  :hook (lsp-managed-mode . (lambda ()
                              (flycheck-add-next-checker 'lsp 'ada-alire))))

(use-package gpr-ts-mode
  :mode ("\\.gpr\\'"))

(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (flycheck-define-checker ada-alire
    "Check Ada code using `alr build`."
    :command ("alr" "build")
    :error-patterns
    ((error line-start (file-name) ":" line ":" column ": error: " (message) line-end)
     (warning line-start (file-name) ":" line ":" column ": warning: " (message) line-end)
     (info line-start (file-name) ":" line ":" column ": (style) " (message) line-end))
    :modes ada-ts-mode)
  (add-to-list 'flycheck-checkers 'ada-alire))

(use-package haskell-mode
  :mode ("\\.hs\\'"))

(use-package lsp-haskell
  :after haskell-mode)

(use-package lsp-mode
  :hook ((c-mode
          c++-mode
          cmake-mode
          java-mode
          kotlin-mode
          ada-ts-mode
          gpr-ts-mode
          haskell-mode) . lsp)
  :commands lsp
  :config
  (setq lsp-clients-clangd-executable "clangd"
        lsp-prefer-flymake nil
        lsp-ui-doc-enable t
        lsp-ui-doc-delay 0.1
        lsp-ui-doc-show-with-cursor nil)
  (define-key evil-normal-state-map (kbd "gd") #'lsp-find-definition)
  (define-key evil-normal-state-map (kbd "gi") #'lsp-find-implementation)
  (define-key evil-normal-state-map (kbd "SPC D") #'lsp-find-type-definition)
  (define-key evil-normal-state-map (kbd "K") #'lsp-ui-doc-glance)
  (define-key evil-normal-state-map (kbd "SPC n d") #'flymake-goto-next-error)
  (define-key evil-normal-state-map (kbd "SPC r n") #'lsp-rename)
  (define-key evil-normal-state-map (kbd "SPC c a") #'lsp-execute-code-action)
  (define-key evil-normal-state-map (kbd "gr") #'lsp-find-references)
  (define-key evil-normal-state-map (kbd "SPC f") #'lsp-format-buffer))

(use-package dap-mode
  :hook (lsp-mode . dap-mode)
  :config
  (define-key evil-normal-state-map (kbd "SPC d t") #'dap-breakpoint-toggle)
  (define-key evil-normal-state-map (kbd "SPC d C-d") #'dap-breakpoint-delete-all)
  (define-key evil-normal-state-map (kbd "SPC d r") #'dap-debug)
  (define-key evil-normal-state-map (kbd "SPC d R") #'dap-debug-restart)
  (define-key evil-normal-state-map (kbd "SPC d q") #'dap-disconnect))

(use-package lsp-java
  :ensure t
  :config
  (add-hook 'java-mode-hook #'lsp))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package consult-lsp
  :after (lsp-mode consult)
  :hook (lsp-mode . my/consult-lsp-setup)
  :commands (consult-lsp-diagnostics)
  :init
  (defun my/consult-lsp-setup ()
    (define-key evil-normal-state-map
      (kbd "SPC l") #'consult-lsp-diagnostics)))

(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  (custom-set-faces
   '(default ((t (:background "#212121"))))))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(setq use-dialog-box nil)
(setq make-backup-files nil
      auto-save-default nil)

(show-paren-mode 1)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)
(column-number-mode 1)
(delete-selection-mode 1)
(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 80)

(setq scroll-margin 5
      scroll-conservatively 100)

(save-place-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)

(global-auto-revert-mode 1)

(use-package which-key
  :config (which-key-mode))

(use-package magit)

(use-package vertico
  :init (vertico-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)))

(use-package marginalia
  :init (marginalia-mode))

(use-package consult)

(use-package embark
  :bind
  (("C-." . embark-act)))

(use-package treemacs
  :commands (treemacs)
  :config
  (define-key evil-normal-state-map (kbd "SPC t") #'treemacs))

(use-package helpful
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   '(((output-dvi has-no-display-manager) "dvi2tty")
     ((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi")
     (output-pdf "Okular") (output-html "xdg-open")))
 '(column-number-mode t)
 '(display-line-numbers-type 'relative)
 '(global-display-line-numbers-mode t)
 '(haskell-process-type 'cabal-repl)
 '(lsp-haskell-formatting-provider "stylish-haskell")
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(ada-ts-mode auctex cmake-mode company consult-lsp doom-themes embark
                 evil-collection evil-commentary evil-surround flycheck
                 gpr-ts-mode haskell-mode helpful kotlin-mode ligature
                 lsp-haskell lsp-java lsp-ui magit marginalia orderless
                 racket-mode tree-sitter-langs vertico))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#212121")))))

(set-face-attribute 'default nil :family "Iosevka Nerd Font Mono" :height 120)

(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers.  You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package company
  :hook (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.1)
  (global-set-key (kbd "C-SPC") #'company-complete))

(use-package org
  :mode (("\\.org\'" . org-mode))
  :config
  (plist-put org-format-latex-options :scale 1.5))

(use-package auctex
  :defer t
  :ensure t
  :hook (LaTeX-mode . company-mode)
  :config
  (defun my/latex-compile () (interactive) (TeX-command "LaTeX" 'TeX-master-file -1))
  (define-key evil-normal-state-map (kbd "SPC l d") #'preview-document)
  (define-key evil-normal-state-map (kbd "SPC l D") #'preview-clearout-document)
  (define-key evil-normal-state-map (kbd "SPC l p") #'preview-at-point)
  (define-key evil-normal-state-map (kbd "SPC l P") #'preview-clearout-at-point)
  (define-key evil-normal-state-map (kbd "SPC l v") #'TeX-view)
  (define-key evil-normal-state-map (kbd "SPC l c") #'my/latex-compile))

(use-package asm-mode
  :mode ("\\.s\\'" . asm-mode)
  :hook (asm-mode . (lambda ()
                      (flycheck-mode 1)
                      (electric-indent-local-mode -1)
                      (setq-local comment-auto-fill-only-comments nil)
                      (setq-local asm-electric-semicolon nil)
                      (advice-add #'asm-comment :override #'self-insert-command)
                      (advice-add #'asm-colon :override #'self-insert-command))))
