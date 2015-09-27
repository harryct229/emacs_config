
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(defvar td/packages)
(setq td/packages
      '(ace-jump-mode
        ag
        anzu
        apache-mode
        async
        base16-theme
        benchmark-init
        bind-key
        cider
        clojure-mode
        comment-dwim-2
        company
        company-statistics
        csv-mode
        dash
        deft
        diff-hl
        diminish
        dockerfile-mode
        emmet-mode
        emms
        emms-player-mpv
        es-mode
        exec-path-from-shell
        expand-region
        f
        flx
        flx-ido
        flycheck
        fullframe
        gitconfig-mode
        groovy-mode
        hideshowvis
        highlight-escape-sequences
        highlight-parentheses
        ibuffer-vc
        ido-ubiquitous
        ido-vertical-mode
        jasminejs-mode
        js2-mode
        json-mode
        markdown-mode
        multiple-cursors
        ob-http
        php-mode
        phpunit
        popwin
        pyvenv
        projectile
        rainbow-mode
        s
        scss-mode
        smart-mode-line
        smartparens
        smex
        solarized-theme
        undo-tree
        use-package
        web-mode
        wgrep
        wgrep-ag
        window-numbering
        yaml-mode
        yasnippet
        ztree
        ))

(defun td/install-packages ()
  (interactive)
  (package-refresh-contents)
  (dolist (p td/packages)
    (unless (package-installed-p p)
      (package-install p))))

(unless (package-installed-p 'use-package)
  (td/install-packages))

(provide 'packages)
