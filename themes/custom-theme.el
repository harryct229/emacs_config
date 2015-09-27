
(deftheme custom
  "My custom faces that fix some theme annoyances.")

(custom-theme-set-faces
 'custom
 ;; Some theme define modeline with boxed border and small font :(
 `(mode-line ((t :box nil :inherit nil :height ,(cdr (assoc :height (face-all-attributes 'default))))))
 `(mode-line-highlight ((t :box nil)))

 `(rainbow-delimiters-depth-1-face ((t :foreground "#d97a35")))
 `(rainbow-delimiters-depth-2-face ((t :foreground "#deae3e")))
 `(rainbow-delimiters-depth-3-face ((t :foreground "#81af34")))
 `(rainbow-delimiters-depth-4-face ((t :foreground "#4e9f75")))
 `(rainbow-delimiters-depth-5-face ((t :foreground "#11535F")))
 `(rainbow-delimiters-depth-6-face ((t :foreground "#00959e")))
 `(rainbow-delimiters-depth-7-face ((t :foreground "#8700ff")))
 `(rainbow-delimiters-unmatched-face ((t :background "#d13120" :underline t)))

 `(diff-hl-insert ((t :inherit nil :background nil :foreground "#81af34")))
 `(diff-hl-delete ((t :inherit nil :background nil :foreground "#ff0000")))
 `(diff-hl-change ((t :inherit nil :background nil :foreground "#deae3e")))
 `(diff-hl-unknown ((t :inherit nil :background nil :foreground "#81af34")))

 `(whitespace-space ((t :background nil)))
 `(whitespace-tab ((t :background nil)))

 `(emmet-preview-input ((t :box nil)))

 `(hl-line ((t :inherit nil :underline nil :foreground nil)))

 `(org-level-1 ((t :weight bold :height 1.3)))
 `(org-level-2 ((t :weight bold :height 1.2)))
 `(org-level-3 ((t :weight bold :height 1.1)))

 `(company-tooltip ((t :background "lightgray" :foreground "black")))
 `(company-tooltip-selection ((t :background "steelblue" :foreground "white")))
 `(company-tooltip-mouse ((t :background "blue" :foreground "white")))
 `(company-tooltip-common ((t :background "lightgray" :foreground "black")))
 `(company-tooltip-common-selection ((t t :background "lightgray" :foreground "black")))
 ;; `(company-tooltip-annotation ((t :background "" :foreground "")))
 `(company-scrollbar-fg ((t :background "black")))
 `(company-scrollbar-bg ((t :background "gray")))
 `(company-preview ((t :background nil :foreround "darkgray")))
 `(company-preview-common ((t :background nil :foreground "darkgray")))
 ;; `(company-preview-search ((t :background "" :foreground "")))
 )

(provide-theme 'custom)
