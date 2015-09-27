;; Buffers with mixed languages are pretty painful in Emacs. I use
;; ~mmm-mode~, which work pretty well for me. The alternative is
;; ~web-mode~, which is more popular but much less flexible.

(use-package mmm-mode
  :init
  (progn
    (setq mmm-global-mode 'maybe)
    (require 'mmm-auto)

    (dolist (pattern '("\\.html" "*twig*" "*tmpl*"
                       "\\.erb" "\\.rhtml$" "\\.ejs$" "\\.hbs$" "\\.ctp$" "\\.tpl$"
                       "/\\(html\\|view\\|template\\|layout\\)/.*\\.php$"))
      (add-to-list 'auto-mode-alist (cons pattern 'html-mode))))

  :config
  (progn
    (require 'mmm-sample)

    (mmm-add-mode-ext-class 'html-mode nil 'html-js)
    (mmm-add-mode-ext-class 'html-mode nil 'html-css)
    (mmm-add-mode-ext-class 'html-mode "\\.ejs\\'" 'ejs)
    (mmm-add-mode-ext-class 'html-mode "\\.\\(erb\\|rhtml\\)\\'" 'erb)
    (mmm-add-mode-ext-class 'html-mode "\\.\\(html\\|html\\.php\\|tmpl\\|ctp\\|tpl\\)\\'" 'html-php)
    (mmm-add-mode-ext-class 'html-mode "/\\(html\\|view\\|template\\|layout\\)/.*\\.php\\'" 'html-php)
    (mmm-add-mode-ext-class 'sh-mode nil 'here-doc)
    (mmm-add-mode-ext-class 'php-mode nil 'here-doc)
    (mmm-add-mode-ext-class 'ruby-mode nil 'here-doc)

    (defun td/mmm-yaml-front-matter-verify ()
      (eq (line-beginning-position) (point-min)))

    (mmm-add-group
     'markdown-extensions
     '((markdown-code-block
        :front "^\\([`~]\\{3,\\}\\)\\([a-zA-Z0-9_-]+\\)$"
        :front-offset (end-of-line 1)
        :save-matches 1
        :back "^~1$"
        :match-submode mmm-here-doc-get-mode
        :insert ((?c markdown-code-block
                     "Code Block Name: " @ "```" str _ "\n" @ "\n" @ "```" "\n" @)))
       (markdown-yaml-front-matter
        :front "^\\(-\\{3,\\}\\)$"
        :front-verify td/mmm-yaml-front-matter-verify
        :front-offset (end-of-line 1)
        :save-matches 1
        :back "^~1$"
        :submode yaml-mode)))
    (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-extensions)))
