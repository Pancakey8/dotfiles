 (require 'autothemer)


(autothemer-deftheme
 pancake
 "Pancake's theme"


 ;; PALETTE
 ((
   ((class color) (min-colors 256)))
   (my-bg         "#fcfcfc")
   (my-bg-alt     "#cacaca")
   (my-disabled   "#dfdfdf")
   (my-text       "#202020")
   (my-blue       "#8cb2e2")
   (my-red        "#d8463e")
   (my-yellow     "#eadb56")
   (my-green      "#58dd75")
   (my-purple     "#c66dd6")
   (my-orange     "#edbd5c"))


 (
  ;; --- CORE UI ---
  (default              (:foreground my-text :background my-bg))
  (cursor               (:background my-text))
  (region               (:background my-bg-alt)) ; Selection
  (fringe               (:background my-bg))
  (line-number          (:foreground my-bg-alt :background my-bg))
  (line-number-current-line (:foreground my-text :background my-bg-alt :weight 'bold))

  ;; --- BORDERS ---
  (vertical-border      (:foreground my-text))
  (window-divider       (:foreground my-text))
  (mode-line            (:foreground my-text :background my-bg-alt :box (:line-width 2 :color my-text)))
  (mode-line-inactive   (:foreground my-disabled :background my-bg :box (:line-width 2 :color my-disabled)))
  (header-line          (:foreground my-text :background my-bg :box (:line-width 2 :color my-text)))


  ;; --- BASIC SYNTAX ---
  (font-lock-comment-face       (:foreground my-disabled :slant 'italic))
  (font-lock-doc-face           (:foreground my-disabled))
  (font-lock-keyword-face       (:foreground my-blue :weight 'bold))
  (font-lock-function-name-face (:foreground my-blue))
  (font-lock-variable-name-face  (:foreground my-text))
  (font-lock-string-face         (:foreground my-green))
  (font-lock-type-face           (:foreground my-purple))
  (font-lock-constant-face       (:foreground my-orange))
  (font-lock-warning-face        (:foreground my-red :weight 'bold))
  (error                        (:foreground my-red :weight 'bold))
  (warning                      (:foreground my-yellow :weight 'bold))
  (success                      (:foreground my-green :weight 'bold))


  ;; --- POPUPS & MENUS ---
  (company-tooltip              (:background my-bg :foreground my-text :box t))
  (company-tooltip-selection    (:background my-bg-alt :foreground my-text :weight 'bold))
  (company-tooltip-scrollbar-thumb (:background my-blue))
  (company-tooltip-scrollbar-track (:background my-bg-alt))
  (ivy-current-match            (:background my-blue :foreground my-text))
  (vertico-current              (:background my-blue :foreground my-text))


  ;; --- ORG MODE ---
  (org-level-1 (:foreground my-blue :weight 'bold :height 1.2))
  (org-level-2 (:foreground my-purple :weight 'bold :height 1.1))
  (org-level-3 (:foreground my-orange :weight 'bold))
  (org-link    (:foreground my-blue :underline t))
  (org-todo    (:foreground my-red :weight 'bold))
  (org-done    (:foreground my-green :weight 'bold))

  ;; --- MAGIT ---
  (magit-section-highlight           (:background my-bg-alt))
  (magit-diff-added                  (:background my-green :foreground my-text))
  (magit-diff-added-highlight        (:background my-green :foreground my-text :weight 'bold))
  (magit-diff-removed                (:background my-red :foreground my-text))
  (magit-diff-removed-highlight      (:background my-red :foreground my-text :weight 'bold))
  (magit-hash                        (:foreground my-disabled))
  ))


(autothemer-colorize)

(provide-theme 'pancake)
