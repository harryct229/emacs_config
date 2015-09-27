
;;; emacs.el --- Emacs configuration generated via Org Babel

;;; Commentary:

;; Do not modify this file by hand.  It was automatically generated
;; from `emacs.org` in the same directory.  See that file for more
;; information.

;;; Code:

;; Configuration group: init-before
(require 'use-package)
(setq gc-cons-threshold (* 4 1024 1024))

(defvar td/vendor-directory (concat user-emacs-directory "vendor/"))
(add-to-list 'load-path td/vendor-directory)
(setq custom-theme-directory (concat user-emacs-directory "themes/"))
(defvar td/data-directory (concat user-emacs-directory "data/"))
(unless (file-exists-p td/data-directory)
  (mkdir td/data-directory))
(require 'benchmark-init)
(add-hook 'after-init-hook 'benchmark-init/deactivate)
(setq delete-by-moving-to-trash t)

(when (eq system-type 'gnu/linux)
  (exec-path-from-shell-initialize))

(when (eq system-type 'darwin)
  (setq ns-use-native-fullscreen nil
        ns-command-modifier 'meta
        ns-option-modifier 'super
        trash-directory "~/.Trash/")

  ;; BSD ls does not support --dired. Use GNU core-utils: brew install coreutils
  (when (executable-find "gls")
    (setq insert-directory-program "gls"))

  ;; Derive PATH by running a shell so that GUI Emacs sessions have access to it
  (exec-path-from-shell-initialize))

(when (eq system-type 'windows-nt)
  (add-to-list 'exec-path "C:\\MinGW\\msys\\1.0\\bin"))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)
(bind-keys :prefix-map td/custom-map :prefix "C-c b")
(require 'cl)

;; Configuration group: appearance
(setq default-frame-alist
      '((left-fringe . 16) (right-fringe . 0)
        (border-width . 0)
        (internal-border-width . 0)
        (left . 256)
        (width . 120) (height . 34)
        (font . "Fira Mono 14")))

(setq-default cursor-type '(bar . 3))

;; Fallback font for missing Unicode glyph
;; (set-fontset-font
;;  nil '(#x0250 . #x02af) (font-spec :family "DejaVu Sans Mono"))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode -1))
(unless (and (eq system-type 'darwin) (display-graphic-p))
  (when (fboundp 'menu-bar-mode)
    (menu-bar-mode -1)))

(setcdr
 (assoc 'truncation fringe-indicator-alist) nil)

(let ((display-table
       (or standard-display-table
           (setq standard-display-table (make-display-table)))))
  (set-display-table-slot display-table 'truncation ?¬)
  (set-display-table-slot display-table 'vertical-border ?\s)
  (set-window-display-table (selected-window) display-table))
(load-theme 'adwaita t)
;; (load-theme 'custom t)
;; (global-hl-line-mode t)
(line-number-mode t)
(column-number-mode t)
;; (if (fboundp 'toggle-frame-maximized)
;;     (add-hook 'emacs-startup-hook 'toggle-frame-maximized))
(defun td/after-make-frame (frame)
  (unless (display-graphic-p frame)
    (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
    (set-face-background 'default "dummy-color" frame)))

(add-hook 'after-make-frame-functions 'td/after-make-frame)
(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)

;; Set message for *scratch* buffer
(setq initial-scratch-message
      (concat ";; Hello, Tung.\n"
              ";; Kẻ thất bại chỉ có một loại,\n"
              ";; chính là loại người từ bỏ trước khi đạt tới thành công.\n"))

;; Use ANSI color in shell
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Never require full word answers
(defalias 'yes-or-no-p 'y-or-n-p)
(use-package smart-mode-line
  :init (sml/setup)
  :config
  (progn
    (add-to-list 'sml/replacer-regexp-list '("^~/Projects/dotfiles/\\(.*\\)/" ":Config:\\1:"))

    (use-package rich-minority
      :init
      (progn
        (add-to-list 'rm-hidden-modes " Undo-Tree")
        (add-to-list 'rm-hidden-modes " yas")
        (add-to-list 'rm-hidden-modes " company")))))
(use-package highlight-parentheses
  :defer 1
  :config (global-highlight-parentheses-mode t))
(use-package paren
  :defer 1
  :init (setq-default show-paren-delay 0)
  :config (show-paren-mode t))
(use-package highlight-escape-sequences
  :defer 1
  :config (hes-mode t))
(use-package rainbow-mode
  :commands rainbow-turn-on
  :init
  (add-hook 'prog-mode-hook 'rainbow-turn-on))
;; (use-package hideshow
;;   :init
;;   :defer t
;;   (progn
;;     (require 'hideshowvis)

;;     ;; We need an around advice here to access the ov internal variable
;;     (defadvice display-code-line-counts
;;         (around td/hideshowvis-no-line-count activate)
;;       ad-do-it
;;       (overlay-put ov 'display " ..."))

;;     (hideshowvis-symbols)

;;     (defun td/toggle-hiding-on-demand ()
;;       (interactive)
;;       (unless hs-minor-mode
;;         (hs-minor-mode t))
;;       (hs-toggle-hiding))
;;     (bind-key "C-c C-SPC" 'td/toggle-hiding-on-demand)))
(use-package origami
  :bind (("C-c C-SPC" . origami-toggle-node)))
(use-package popwin
  :commands popwin-mode
  :defer 1
  :config
  (progn
    (bind-key "C-x p" popwin:keymap)

    (mapc (lambda (c)
            (add-to-list 'popwin:special-display-config c))
          '((occur-mode :noselect nil)
            ("*Org Agenda*" :width 60 :position right :dedicated t :stick t)
            ("*Compile-Log*" :height 20 :noselect t)
            ("*Ido Completions*" :noselect t :height 15)
            ("*cider-error*" :height 15 :stick t)
            ("*cider-doc*" :height 15 :stick t)
            ("*cider-src*" :height 15 :stick t)
            ("*cider-result*" :height 15 :stick t)
            ("*cider-macroexpansion*" :height 15 :stick t)
            (shell-mode :height 15)
            (ag-mode :height 15)))

    (popwin-mode 1)))
(use-package diff-hl
  :defer 1
  :config
  (progn
    (define-fringe-bitmap 'td/diff-hl-bmp [192] 1 16 '(top t))
    (defun td/diff-hl-bmp (type pos) 'td/diff-hl-bmp)

    (setq diff-hl-draw-borders nil
          diff-hl-fringe-bmp-function #'td/diff-hl-bmp)

    (set-face-attribute 'diff-hl-insert nil :background nil :foreground "#81af34")
    (set-face-attribute 'diff-hl-delete nil :background nil :foreground "#ff0000")
    (set-face-attribute 'diff-hl-change nil :background nil :foreground "#deae3e")

    ;; (set-face-attribute 'diff-hl-insert nil :background nil)
    ;; (set-face-attribute 'diff-hl-delete nil :background nil)
    ;; (set-face-attribute 'diff-hl-change nil :background nil)

    (defun diff-hl-overlay-modified (ov after-p beg end &optional len)
      "Markers disappear and reapear is kind of annoying to me.")

    (global-diff-hl-mode t)))
(use-package fullframe
  :config
  (progn
    (fullframe ibuffer ibuffer-quit)

    (defvar td/last-window-configuration nil
      "Last window configuration.")

    ;; Ediff has multiple frames, and does not play nice with fullframe API
    (defun td/store-window-configuration ()
      "Store current window configuration."
      (setq td/last-window-configuration (current-window-configuration)))

    (defun td/restore-window-configuration ()
      "Restore current window configuration."
      (set-window-configuration td/last-window-configuration)))

    (add-hook 'ediff-before-setup-hook #'td/store-window-configuration)
    (add-hook 'ediff-quit-hook #'td/restore-window-configuration 'append)
    (add-hook 'ediff-suspend-hook #'td/restore-window-configuration 'append))

;; Configuration group: editing
(setq default-input-method 'vietnamese-telex)
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(defadvice ansi-term (after advise-ansi-term-coding-system)
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(ad-activate 'ansi-term)
(use-package undo-tree
  :init (global-undo-tree-mode t)
  :config
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-relative-timestamps t
        undo-tree-history-directory-alist
        (list (cons "." (expand-file-name "undos" td/data-directory)))))
(use-package vc-hooks
  :defer t
  :config (setq vc-follow-symlinks t))

(use-package vc-dir
  :defer t
  :config
  (progn
    (defun td/vc-git-command (verb fn)
      (let* ((args (vc-deduce-fileset nil t))
             (backend (car args))
             (files (nth 1 args)))
        (if (eq backend 'Git)
            (progn
              (funcall fn files)
              (message (concat verb " "
                               (number-to-string (length files))
                               " file(s).")))
          (message "Not in a vc git buffer."))))

    (defun td/vc-git-add (&optional revision args comment)
      (interactive "P")
      (td/vc-git-command "Staged" 'vc-git-register))

    (defun td/vc-git-reset (&optional revision args comment)
      (interactive "P")
      (td/vc-git-command
       "Unstaged"
       (lambda (files) (vc-git-command nil 0 files "reset" "-q" "--"))))

    (defun td/vc-git-amend (&optional revision args comment)
      (interactive "P")
      (td/vc-git-command
       "Ammended"
       (lambda (files)
         (vc-git-command nil 0 files
                         "commit" "--amend" "--reuse-message=HEAD"))))

    (defadvice vc-dir-refresh
        (after td/vc-hide-up-to-date-after-refresh activate)
      (vc-dir-hide-up-to-date))

    (bind-keys :map vc-dir-mode-map
               ("r" . vc-revert-buffer)
               ("a" . td/vc-git-add)
               ("u" . td/vc-git-reset)
               ("A" . td/vc-git-amend))

    (bind-keys :map vc-prefix-map
               ("r" . vc-revert-buffer)
               ("a" . td/vc-git-add)
               ("u" . td/vc-git-reset))))
(use-package isearch
  :bind (("C-s" . isearch-forward-regexp)
         ("C-r" . isearch-backward-regexp))
  :init
  (progn
    (defun td/isearch-message (&optional c-q-hack ellipsis)
      "Cursor flashing in the echo area makes me crazy."
      (isearch-message c-q-hack nil))

    (setq lazy-highlight-initial-delay 0
          isearch-message-function #'td/isearch-message)

    ;;(require 'isearch-diacritics-fold)
    ))
(use-package anzu
  :init (global-anzu-mode t)
  :bind (("M-%" . anzu-query-replace-regexp)
         ("C-c r" . anzu-query-replace-at-cursor-thing)
         ("C-c R" . td/anzu-query-replace-all-at-cursor))
  :config
  (progn
   (setq anzu-mode-lighter ""
         anzu-search-threshold 256
         anzu-minimum-input-length 3)

   (defun td/anzu-query-replace-all-at-cursor ()
     (interactive)
     (let ((anzu-replace-at-cursor-thing 'page))
       (call-interactively 'anzu-query-replace-at-cursor-thing)))))
(setq-default indent-tabs-mode nil)
(setq require-final-newline t) ; auto-insert final newlines in all files

(use-package whitespace
  :commands (whitespace-cleanup
             whitespace-mode)
  :bind ("C-c w" . whitespace-mode)
  :config
  (progn
    (add-to-list 'whitespace-display-mappings
                 '(newline-mark ?\n [?\u00AC ?\n] [?$ ?\n]) t)

    (setq whitespace-line-column nil
          whitespace-style
          '(face
            tabs tab-mark
            spaces space-mark
            newline newline-mark
            trailing lines-tail
            space-before-tab space-after-tab))

    (add-hook 'before-save-hook #'whitespace-cleanup)
    (add-hook 'before-save-hook #'delete-trailing-whitespace)))
(setq-default
 comment-auto-fill-only-comments t
 fill-column 75)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'prog-mode-hook 'turn-on-auto-fill)
(when (executable-find "aspell")
  (use-package ispell
    :bind ("<f8>" . ispell-word)
    :init (setq-default ispell-program-name "aspell"
                        ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")
                        ispell-skip-html t
                        ispell-silently-savep t
                        ispell-really-aspell t))

  (use-package flyspell
    :defer t
    :init (add-hook 'text-mode-hook 'flyspell-mode)
    :config
    (progn
      (require 'flyspell-ignore-faces)
      (put 'org-mode 'flyspell-mode-predicate 'td/flyspell-check-p))))
(use-package smartparens
  :defer t
  :init (smartparens-global-mode t)
  :config
  (progn
    (require 'smartparens-config)
    (sp-use-smartparens-bindings)

    (setq sp-autoinsert-if-followed-by-same 1
          sp-autoescape-string-quote nil
          sp-highlight-pair-overlay nil)

    (sp-pair "{" nil
             :post-handlers '(:add ("||\n[i]" "RET") ("| " "SPC")))
    (sp-pair "[" nil
             :post-handlers '(:add ("||\n[i]" "RET") ("| " "SPC")))
    (sp-pair "(" nil
             :post-handlers '(:add ("||\n[i]" "RET") ("| " "SPC")))

    (defun sp-web-mode-is-code-context (id action context)
      (and (eq action 'insert)
           (or (get-text-property (point) 'part-side)
               (get-text-property (point) 'block-side))))

    (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))))
(use-package comment-dwim2
  :commands comment-dwim2
  :init
  (progn
    (setq comment-style 'multi-line)
    (bind-key "C-l" #'comment-dwim-2)))
(pending-delete-mode t)

(setq kill-ring-max 4096
      kill-whole-line t)

(use-package expand-region
  :bind ("M--" . er/expand-region))

(use-package multiple-cursors
  :commands (mc/mark-previous-like-this
             mc/mark-next-like-this
             mc/skip-to-previous-like-this
             mc/skip-to-next-like-this
             mc/mark-all-like-this)
  :init
  (bind-keys ("M-C-9" . mc/mark-previous-like-this)
             ("M-C-0" . mc/mark-next-like-this)
             ("M-(" . mc/skip-to-previous-like-this)
             ("M-)" . mc/skip-to-next-like-this)
             ("M-C-a" . mc/mark-all-like-this)))

(bind-key "C-x SPC" 'set-rectangular-region-anchor)
(bind-key "M-`" #'other-frame)
;; (td/bind "C-M-f" #'td/toggle-fullscreen)

(defun end-with-newline ()
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(defun end-with-semicolon ()
  (interactive)
  (move-end-of-line 1)
  (insert ";"))

(bind-key "RET" #'newline-and-indent)
(bind-key "M-RET" #'end-with-newline)
(bind-key "M-;" #'end-with-semicolon)


(defun td/next-ten-visual-lines ()
  (interactive)
  (next-line 10))

(defun td/previous-ten-visual-lines ()
  (interactive)
  (previous-line 10))

(bind-key "M-n" #'td/next-ten-visual-lines)
(bind-key "M-p" #'td/previous-ten-visual-lines)


(autoload 'zap-up-to-char "misc" nil :interactive)
(autoload 'zap-to-char "misc" nil :interactive)

(bind-key "M-z" #'zap-up-to-char)
(bind-key "M-Z" #'zap-to-char)


(defun td/cleanup-buffer ()
  (interactive)
  (indent-region (point-min) (point-max))
  (untabify (point-min) (point-max))
  (whitespace-cleanup))

(bind-key "M-=" #'td/cleanup-buffer)


(defun td/join-next-line ()
  (interactive)
  (join-line -1))

(bind-key "M-J" #'td/join-next-line)


(defun td/kill-region-or-word ()
  (interactive)
  (call-interactively
   (if (region-active-p) 'kill-region 'backward-kill-word)))

(bind-key "C-w" #'td/kill-region-or-word)


(defun td/eval-and-replace ()
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(bind-key "C-c C-e" #'td/eval-and-replace)


(defun extract-variable (begin end var)
  (interactive "r\nsVariable name: ")
  (kill-region begin end)
  (insert var)
  (forward-line -1)
  (newline-and-indent)
  (insert var " = ")
  (yank))

(defun inline-variable ()
  (interactive)
  (let ((var (current-word)))
    (re-search-forward "= ")
    (let ((value (buffer-substring (point) (point-at-eol))))
      (kill-whole-line)
      (search-forward var)
      (replace-match value))))

(defun align=: (&optional args)
  "Align region to equal signs or colon"
  (interactive)
  (with-region-or-current-line
   (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)[=|:]" 1 1)))


(defun open-thing-at-point ()
  (interactive)
  (cond
   ((-when-let (url (thing-at-point 'url))
      (browse-url url)))
   ((-when-let (email (thing-at-point 'email))
      (browse-url (format "mailto:%s" email))))
   ((-when-let (path (thing-at-point 'filename))
      (if (file-exists-p path)
          (find-file path)
        (if (file-exists-p (concat path ".el"))
            (find-file (concat path ".el"))
          (when (y-or-n-p (format "Create %s?" path))
            (find-file path))))))))

(bind-key "M-o" 'open-thing-at-point)

(defun char-upcasep (letter)
  (eq letter (upcase letter)))

;; TOOD: find appropriate key binding for these functions

(defun capitalize-word-toggle ()
  (interactive)
  (let ((start (car (save-excursion
                      (backward-word)
                      (bounds-of-thing-at-point 'symbol)))))
    (if start
        (save-excursion
          (goto-char start)
          (funcall (if (char-upcasep (char-after))
                       'downcase-region
                     'upcase-region)
                   start (1+ start)))
      (capitalize-word -1))))

(defun upcase-word-toggle ()
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'symbol))
        beg end
        (regionp (if (eq this-command last-command)
                     (get this-command 'regionp)
                   (put this-command 'regionp nil))))
    (cond
     ((or (region-active-p) regionp)
      (setq beg (region-beginning)
            end (region-end))
      (put this-command 'regionp t))
     (bounds
      (setq beg (car bounds)
            end (cdr bounds)))
     (t
      (setq beg (point)
            end (1+ beg))))
    (save-excursion
      (goto-char (1- beg))
      (and (re-search-forward "[A-Za-z]" end t)
           (funcall (if (char-upcasep (char-before))
                        'downcase-region
                      'upcase-region)
                    beg end)))))


(defun find-file-sudo (&optional arg)
  (interactive)
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file
     (concat "/sudo:root@localhost:" buffer-file-name))))


(defun td/before-save-make-directories ()
  (let ((dir (file-name-directory buffer-file-name)))
    (when (and buffer-file-name (not (file-exists-p dir)))
      (make-directory dir t))))

(add-hook 'before-save-hook #'td/before-save-make-directories)


(defun td/after-save-auto-chmod ()
  (when (and (> (length (buffer-string)) 5)
             (string-equal "#!" (buffer-substring-no-properties 1 4)))
    (shell-command
     (format "chmod u+x %s"
             (shell-quote-argument (buffer-file-name))))))

(add-hook 'after-save-hook #'td/after-save-auto-chmod)
(defun delete-current-buffer-file ()
  (interactive)
  (let ((filename (buffer-file-name)))
    (when (and filename (file-exists-p filename))
      (delete-file filename)
      (kill-this-buffer))))

(defun rename-current-buffer-file (new-name)
  (interactive
   (list (read-string "New name: " (buffer-name))))
  (let ((filename (buffer-file-name)))
    (when (and filename (file-exists-p filename))
      (if (get-buffer new-name)
          (error "Buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name t)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun write-file-copy (filename)
  (interactive "F")
  (save-restriction (widen)
                    (write-region (point-min) (point-max) filename)))

(defun write-timestamped-file-copy (filename)
  (interactive "F")
  (let ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
        (filename-head (file-name-sans-extension filename))
        (filename-ext (file-name-extension filename t)))
    (write-file-copy (expand-file-name (concat filename-head "_" timestamp filename-ext)))))

(defun write-timestamped-current-file-copy ()
  (interactive)
  (write-timestamped-file-copy (buffer-file-name)))

(bind-keys :map td/custom-map
           ("r" . rename-current-buffer-file)
           ("d" . delete-current-buffer-file)
           ("t" . write-timestamped-current-file-copy))
(use-package yasnippet
  :commands yas-global-mode
  :init
  (progn
    (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
    (yas-global-mode t))
  :config
  (progn
    (setq yas-prompt-functions
          '(yas-ido-prompt yas-completing-prompt yas-no-prompt)
          ;; Suppress excessive log messages
          yas-verbosity 1
          ;; I am a weird user, I use SPACE to expand my
          ;; snippets, this save me from triggering them accidentally.
          yas-expand-only-for-last-commands
          '(self-insert-command org-self-insert-command))

    (unbind-key "TAB" yas-minor-mode-map)
    (unbind-key "<tab>" yas-minor-mode-map)
    (bind-key "SPC" 'yas-expand yas-minor-mode-map)))
(use-package ediff
  :defer t
  :init
  (progn
    (defun td/ediff-from-command-line (switch)
      (let ((file-a (pop command-line-args-left))
            (file-b (pop command-line-args-left)))
        (ediff file-a file-b)))

    (add-to-list 'command-switch-alist '("diff" . td/ediff-from-command-line)))
  :config
  (progn
    (setq ediff-window-setup-function 'ediff-setup-windows-plain
          ediff-split-window-function 'split-window-horizontally)))
(use-package flycheck
  :commands (global-flycheck-mode flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers
                '(html-tidy go-build emacs-lisp-checkdoc)))

;; Configuration group: navigation-completion
(use-package which-func
  :defer 1
  :config
  (which-function-mode t))

(use-package imenu
  :defer t
  :bind ("C-c i" . imenu)
  :config
  (setq imenu-auto-rescan t))
(use-package company
  :init
  (progn
    (setq company-backends
          '(company-anaconda
            company-css
            (;company-yasnippet
             company-capf
             company-dabbrev-code
             company-keywords)))
    (global-company-mode t))
  :config
  (progn
    (use-package company-lines
      :commands (company-lines))

    (setq completion-cycle-threshold 5)

    (setq company-idle-delay 0.2
          company-auto-complete nil
          company-selection-wrap-around t
          company-echo-delay 0
          company-tooltip-align-annotations t
          company-show-numbers t
          company-minimum-prefix-length 3

          company-auto-complete-chars
          '(?\ ?\( ?\) ?. ?\" ?$ ?\' ?< ?> ?| ?!)

          company-transformers
          '(company-sort-by-occurrence)

          company-frontends
          '(company-pseudo-tooltip-unless-just-one-frontend
            company-echo-metadata-frontend
            company-preview-if-just-one-frontend))

    (bind-keys :prefix-map td/completion-map
               :prefix "M-;"
               ("s" . company-ispell)
               ("f" . company-files)
               ("l" . company-lines))

    (bind-keys :map company-active-map
               ([escape] . company-abort)
               ("<tab>" . company-complete-dwim)
               ("<backtab>" . company-select-previous)
               ("C-n" . company-select-next)
               ("C-p" . company-select-previous)
               ("C-s" . company-filter-candidates)
               ("C-l" . company-show-location)
               ("C-j" . company-complete-common)
               ("C-d" . company-show-doc-buffer))

    (defun company-complete-dwim (&optional arg)
      (interactive "P")
      (let ((pos (point)))
        (indent-according-to-mode)
        (when (and (= pos (point)) (looking-at "\\_>"))
          (if (eq last-command 'company-complete-dwim)
              (company-select-next)
            (company-complete-common)))))

    (bind-keys :map company-mode-map
               ([remap indent-for-tab-command] . company-complete-dwim))))
(use-package savehist
  :defer t
  :init
  (progn
    (setq savehist-file (expand-file-name "savehist" td/data-directory))
    (savehist-mode t)))
(use-package ido
  :commands (ido-switch-buffer
             ido-find-file)
  :init
  (progn
    (ido-mode t)
    (ido-everywhere t)
    (ido-ubiquitous-mode t)
    (ido-vertical-mode)
    (flx-ido-mode t))

  :config
  (progn
    (setq ido-save-directory-list-file
          (expand-file-name "ido.last" td/data-directory)
          ido-enable-flex-matching t
          ido-create-new-buffer 'always
          ido-case-fold t
          ido-file-extensions-order
          '(".rb" ".py" ".clj" ".cljs" ".el" ".coffee" ".js" ".ts"
            ".scss" ".php" ".html" t)
          ido-default-buffer-method 'samewindow
          ido-vertical-define-keys nil
          flx-ido-threshold 2048)

    (add-to-list 'ido-ignore-files "\\.DS_Store")
    (add-to-list 'ido-ignore-files "__pycache__")

    (defun td/minibuffer-home ()
      (interactive)
      (if (looking-back "/")
          (insert "~/")
        (call-interactively 'self-insert-command)))

    (defun td/minibuffer-insert-word-at-point ()
      (interactive)
      (let (word beg)
        (with-current-buffer (window-buffer (minibuffer-selected-window))
          (setq word (thing-at-point 'word)))
        (insert word)))

    (defun ido-goto-line ()
      (interactive)
      (let* ((lines (split-string (buffer-string) "[\n\r]"))
             (choices (-remove (lambda (l) (zerop (length l))) lines))
             (line (ido-completing-read "Line: " choices)))
        (push-mark)
        (goto-line (+ 1 (-elem-index line lines)))))

    (bind-key "C-c l" #'ido-goto-line)

    (defun td/ido-hook ()
      (bind-keys :map ido-completion-map
                 ("C-h" . delete-backward-char)
                 ("ESC" . ido-exit-minibuffer)
                 ("C-w" . ido-delete-backward-updir)
                 ("C-n" . ido-next-match)
                 ("C-p" . ido-prev-match)
                 ("TAB" . ido-complete)
                 ("C-l" . td/minibuffer-insert-word-at-point)
                 ("~" . td/minibuffer-home)))

    (add-hook 'ido-setup-hook #'td/ido-hook)))
(use-package smex
  :bind (("M-m" . smex)
         ("M-M" . smex-major-mode-commands))
  :init
  (progn
    (setq smex-save-file (expand-file-name "smex-items" td/data-directory)
          smex-flex-matching t)
    (smex-initialize)))
(use-package saveplace
  :init
  (progn
    (setq-default save-place t)))
(use-package window-numbering
  :init (window-numbering-mode t)
  :config
  (progn
    (defadvice window-numbering-get-number-string
      (after td/custom-window-numbering-mode-line-string activate)
      (setq ad-return-value (format "[%s] " ad-return-value)))))

(defun kill-buffer-and-window-silently ()
  (interactive)
  (ignore-errors (kill-buffer-and-window)))

(bind-keys ("C-c q" . delete-frame)
           ("C-c Q" . delete-window)
           ("C-c k" . kill-buffer-and-window-silently))
(setq confirm-nonexistent-file-or-buffer nil)

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward
        uniquify-separator " - "
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))

(use-package ibuffer
  :commands ibuffer
  :bind ("C-x C-b" . ibuffer)
  :init
  (progn
    (setq ibuffer-saved-filter-groups
          '(("Config" (or
                       (filename . ".dots/")
                       (filename . ".emacs.d/")))
            ("Shell"  (or
                       (mode . eshell-mode)
                       (mode . shell-mode)))
            ("Dired"  (mode . dired-mode))
            ("Prose"  (or
                       (mode . tex-mode)
                       (mode . plain-tex-mode)
                       (mode . latex-mode)
                       (mode . rst-mode)
                       (mode . markdown-mode)))
            ("Org"    (mode . org-mode))
            ("Gnus"   (or
                       (mode . message-mode)
                       (mode . gnus-group-mode)
                       (mode . gnus-summary-mode)
                       (mode . gnus-article-mode)))
            ("Emacs"  (name . "^\\*.*\\*$")))
          ibuffer-show-empty-filter-groups nil
          ibuffer-expert t)

    (use-package ibuffer-vc
      :commands ibuffer-vc-generate-filter-groups-by-vc-root
      :config
      (progn
        (defun td/ibuffer-apply-filter-groups ()
          "Combine my saved ibuffer filter groups with those generated
     by `ibuffer-vc-generate-filter-groups-by-vc-root'"
          (interactive)
          (setq ibuffer-filter-groups
                (append (ibuffer-vc-generate-filter-groups-by-vc-root)
                        ibuffer-saved-filter-groups))
          (message "ibuffer-vc: groups set")
          (let ((ibuf (get-buffer "*Ibuffer*")))
            (when ibuf
              (with-current-buffer ibuf
                (pop-to-buffer ibuf)
                (ibuffer-update nil t)))))

        (add-hook 'ibuffer-hook 'td/ibuffer-apply-filter-groups)))))
;; (use-package midnight)
(defun td/recent-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(bind-keys :map td/custom-map
           ("b" . td/recent-buffer))
(use-package ace-jump-mode
  :bind (("M-'" . ace-jump-word-mode)
         ("M-C-'" . ace-jump-char-mode)))
(use-package projectile
  :defer t
  :init (projectile-global-mode t)
  :config
  (progn
    (setq projectile-enable-idle-timer t
          ;; Use static mode line here to eliminate GC. SML already
          ;; displays the current project anyway.
          projectile-mode-line " Proj")

    (bind-keys ("M-l" . projectile-find-file)
               ([remap projectile-ack] . projectile-ag)
               ([remap projectile-grep] . projectile-ag))

    (add-to-list 'projectile-globally-ignored-directories "__pycache__")))

(use-package wgrep
  :defer t
  :init (add-hook 'ag-mode-hook #'wgrep-setup))
(defun td/custom-font-lock-hightlights ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t)))
  (font-lock-add-keywords
   nil '(("%\\(?:[-+0-9\\$.]+\\)?[bdiuoxXDOUfeEgGcCsSpn]"
          0 font-lock-preprocessor-face t))))

(add-hook 'prog-mode-hook #'td/custom-font-lock-hightlights)

(defun td/occur-todos ()
  (interactive)
  (occur "TODO"))
(setq tags-revert-without-query t)

;; Configuration group: programming
(use-package indent-guide
  :commands (indent-guide-mode)
  :init
  (add-hook 'python-mode-hook #'indent-guide-mode))

(use-package electric-case
  :init
  (progn

    (setq electric-case-convert-calls t)

    (defun td/electric-case-js-init ()
      "TODO: documentation."
      )

    (defun td/electric-case-python-init ()
      "TODO: documentation."
      )

    (add-hook 'java-mode-hook #'electric-case-java-init)
    ;; (add-hook 'js2-mode-hook #'electric-case-java-init)
    ;; (add-hook 'js-mode-hook #'electric-case-java-init)
    (add-hook 'python-mode-hook #'td/electric-case-python-init)))
(use-package compile
  :defer t
  :init
  (progn
    (defun recompile-with-last-configuration ()
      (interactive)
      (save-some-buffers)
      (when compilation-last-buffer
        (with-current-buffer compilation-last-buffer
          (call-interactively 'recompile))))

    (bind-key "C-c m" 'recompile-with-last-configuration))
  :config
  (progn
    (setq compilation-scroll-output t)

    (defun td/colorize-compilation-buffer ()
      (toggle-read-only)
      (ansi-color-apply-on-region (point-min) (point-max))
      (toggle-read-only))

    (add-hook 'compilation-filter-hook #'td/colorize-compilation-buffer)))
(use-package emacs-lisp-mode
  :mode ("Cask" . emacs-lisp-mode))

(use-package eldoc
  :commands turn-on-eldoc-mode
  :init (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  :config
  (progn
    (defun eldoc-message-now ()
      (interactive))

    (setq eldoc-idle-delay 0)

    (defun eldoc--message-command-p (command)
      (eq command 'eldoc-message-now))

    (bind-key "C-c d" #'eldoc-message-now)

    (eldoc-add-command 'eldoc-message-now)))
(use-package ruby-mode
  :mode (("\\.rake$" . ruby-mode)
         ("\\.gemspec$" . ruby-mode)
         ("\\.ru$" . ruby-mode)
         ("Rakefile$" . ruby-mode)
         ("Gemfile$" . ruby-mode)
         ("Capfile$" . ruby-mode)
         ("Puppetfile$" . ruby-mode)
         ("Guardfile$" . ruby-mode)
         ("Vagrantfile$" . ruby-mode)))
(use-package js
  :defer t
  :config
  (setq js-indent-level 2
        js-expr-indent-offset 2
        js-flat-functions t))

(use-package js2-mode
  :mode "\\.js$"
  :config
  (progn
    (require 'js-indentation)

    (setq-default
     js2-basic-offset 2
     js2-highlight-level 3
     js2-idle-timer-delay 0
     js2-mode-show-parse-errors nil
     js2-strict-missing-semi-warning nil
     js2-indent-switch-body t
     js2-bounce-indent-p nil
     js2-include-node-externs t
     js2-global-externs
     '("jQuery" "Zepto" "$" "location" "Image" "describe" "it" "goog"
       "require" "define" "exports"))))
(use-package python
  :config
  (progn
    (setq python-indent-guess-indent-offset nil)

    (defun td/setup-python ()
      (interactive)
      (anaconda-mode t)
      (eldoc-mode t)
      ;; (add-to-list 'company-backends 'company-anaconda)
      )

    (add-hook 'python-mode-hook #'td/setup-python)))
(use-package php-mode
  :mode "\\.php\\'"
  :config
  (progn
    (setq php-template-compatibility nil
          php-manual-path "~/local/docs/php"
          php-mode-warn-if-mumamo-off nil
          php-mode-coding-style 'drupal)))
(use-package web-mode
  :mode (("\\.phtml\\'" . web-mode)
         ("/\\(views\\|html\\|templates\\)/.*\\.php\\'" . web-mode)
         ("\\.tpl\\'" . web-mode)
         ("\\.[gj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.jinja2\\'" . web-mode)
         ("\\.html?" . web-mode)
         ("\\.hbs\\'" . web-mode))
  :config
  (progn
    (setq web-mode-markup-indent-offset 2)
    (add-hook 'web-mode-hook #'turn-off-auto-fill)
    (add-to-list 'web-mode-imenu-regexp-list
                  '(" \\(ng-[a-z]*\\)=\"\\([a-zA-Z0-9]*\\)" 1 2 "="))))
(use-package emmet-mode
  :commands emmet-mode
  :init
  (progn
    (add-hook 'sgml-mode-hook #'emmet-mode)
    (add-hook 'web-mode-hook #'emmet-mode)
    (add-hook 'css-mode-hook #'emmet-mode))
  :config
  (progn
    (setq emmet-indentation 2
          emmet-preview-default nil
          emmet-insert-flash-time 0.1)

    (defadvice emmet-preview
      (after td/emmet-preview-hide-tooltip activate)
      (overlay-put emmet-preview-output 'before-string nil))))
(defun td/css-imenu-expressions ()
  (add-to-list 'imenu-generic-expression '("Section" "^.*\\* =\\(.+\\)$" 1) t))

(use-package css-mode
  :mode "\\.css\\'"
  :init
  (progn
    (setq-default css-indent-offset 2)
    (add-hook 'css-mode-hook #'td/css-imenu-expressions)))

(use-package scss-mode
  :mode "\\.scss\\'"
  :init
  (progn
    (setq scss-compile-at-save nil
          css-indent-offset 2)
    (add-hook 'scss-mode-hook #'td/css-imenu-expressions)))

(use-package less-css-mode
  :mode "\\.less\\'"
  :init
  (progn
    ;; TODO: customize `less-css-indent-line' to support nested ruleset
    (add-hook 'less-css-mode-hook #'td/css-imenu-expressions)))
(use-package markdown-mode
  :mode (("\\.md$" . markdown-mode)
         ("\\.mkd$" . markdown-mode)
         ("\\.markdown$" . markdown-mode))
  :config
  (progn
    (setq markdown-command "redcarpet"
          markdown-enable-math t
          markdown-header-face '(:inherit font-lock-function-name-face :weight bold)
          markdown-header-face-1 '(:inherit markdown-header-face :height 2.0)
          markdown-header-face-2 '(:inherit markdown-header-face :height 1.6)
          markdown-header-face-3 '(:inherit markdown-header-face :height 1.4)
          markdown-header-face-4 '(:inherit markdown-header-face :height 1.2))

    (add-hook 'markdown-mode-hook #'turn-on-flyspell)
    (add-hook 'markdown-mode-hook #'turn-on-auto-fill)))
(use-package nxml-mode
  :defer t
  :config
  (progn
    (defun nxml-where ()
      "Display the hierarchy of XML elements the point is on as a path."
      (interactive)
      (let ((path nil))
        (save-excursion
          (save-restriction
            (widen)
            (while (and (< (point-min) (point)) ;; Doesn't error if point is at beginning of buffer
                        (condition-case nil
                            (progn
                              (nxml-backward-up-element) ; always returns nil
                              t)
                          (error nil)))
              (setq path (cons (xmltok-start-tag-local-name) path)))
            (if (called-interactively-p t)
                (message "/%s" (mapconcat 'identity path "/"))
              (format "/%s" (mapconcat 'identity path "/")))))))

    (bind-key "C-c C-p" 'nxml-where)))
(use-package fish-mode
  :mode (("\\.fish$" . fish-mode))
  :config
  (progn

    (defun td/setup-fish-mode ()
      (setq-local tab-width 2))

    (add-hook 'fish-mode-hook #'td/setup-fish-mode)))
(use-package sh-script
  :defer t
  :init
  (setq-default sh-basic-offset 2))

;; Configuration group: management
(use-package deft
  :bind ("C-c b d" . deft)
  :config
  (progn
    (setq deft-extension "org"
          deft-text-mode 'org-mode
          deft-directory "~/Documents"
          deft-strip-title-regexp "\\(?:^%+\\|#\\+[[:alpha:]]+:\\|^[#* ]+\\|-\\*-[[:alpha:]]+-\\*-\\|#+$\\)")))
(use-package eshell
  :defer t
  :init
  (progn
    (setq eshell-list-files-after-cd t)

    (defun td/find-eshell ()
      (interactive)
      (if (get-buffer "*eshell*")
          (switch-to-buffer-other-window "*eshell*")
        (split-window-sensibly (selected-window))
        (other-window 1)
        (eshell)))

    (defun td/find-eshell-default-directory ()
      (interactive)
      (let ((cwd default-directory))
        (td/find-eshell)
        (eshell/cd cwd)))

    (bind-keys :map td/custom-map
               ("t" . td/find-eshell)
               ("s" . td/find-eshell-default-directory))
    :config
    (progn
      (defmacro td/with-face (str &rest properties)
        `(propertize ,str 'face (list ,@properties)))

      (defun td/eshell-pwd ()
        (replace-regexp-in-string
         (regexp-quote (expand-file-name "~"))
         "~"
         (eshell/pwd)))

      (defun td/eshell-prompt ()
        (format
         "\n%s@%s in %s\n%s "
         (td/with-face user-login-name :foreground "#dc322f")
         (td/with-face (or (getenv "HOST") system-name) :foreground "#b58900")
         (td/with-face (td/eshell-pwd) :foreground "#859900")
         (if (= (user-uid) 0) (with-face "#" :foreground "red") "$")))

      (defalias 'eshell/e 'find-file-other-window)

      (defun eshell/open (args)
        (interactive)
        (shell-command
         (concat (case system-type
                   ((darwin) "open")
                   ((windows-nt) "start")
                   (t "xdg-open"))
                 (format " %s" args))))

      (setq eshell-prompt-function #'td/eshell-prompt
            eshell-prompt-regexp "^[^#$\\n]*[#$] "
            eshell-highlight-prompt nil))))
(use-package tramp
  :defer t
  :config
  (progn
    (setq password-cache-expiry nil
          tramp-debug-buffer t
          tramp-default-method "ftp")

    (add-to-list 'auth-sources "~/.emacs.d/authinfo.gpg")
    (setq ange-ftp-netrc-filename "~/.emacs.d/authinfo.gpg")))
(use-package dired
  :defer t
  :config
  (progn
    (setq dired-listing-switches "-alh"
          dired-recursive-copies 'always
          dired-recursive-deletes 'always)

    (defun td/dired-back-to-top ()
      (interactive)
      (goto-char (point-min))
      (dired-next-line 4))

    (defun td/dired-jump-to-bottom ()
      (interactive)
      (goto-char (point-max))
      (dired-next-line -1))

    (bind-keys :map dired-mode-map
               ("M-<" . td/dired-back-to-top)
               ("M->" . td/dired-jump-to-bottom))))
(use-package recentf
  :defer 1
  :config
  (progn
    (setq recentf-auto-cleanup "9:00pm"
          recentf-max-saved-items 256)

    (add-hook 'server-visit-hook #'recentf-save-list)

    (add-to-list 'recentf-exclude "elpa")

    (defun recentf-ido-find-file ()
      "Find a recent file using Ido."
      (interactive)
      (let ((file (ido-completing-read "Recent file: " recentf-list nil t)))
        (when file
          (find-file file))))

    (bind-key "C-c ;" #'recentf-ido-find-file)

    (recentf-mode t)))
(use-package emms
  :defer t
  :config
  (progn
    (require 'emms-setup)

    (emms-standard)
    (emms-default-players)

    (setq emms-source-file-default-directory "~/Music/"
          emms-info-asynchronously t
          emms-show-format "♫ %s"
          emms-repeat-playlist t)

    (require 'emms-mode-line)
    (require 'emms-mode-line-icon)
    (setq emms-mode-line-format " [%s] ")
    (emms-mode-line 1)

    (define-emms-simple-player afplay '(file)
      (regexp-opt '(".mp3" ".m4a" ".aac"))
      "afplay")
    (setq emms-player-list `(,emms-player-afplay))))

(use-package emms-browser
  :bind (("<f7>" . emms-smart-browse))
  :config
  (setq emms-browser-covers
        '("cover_small.jpg" "cover_medium.jpg" "cover.jpg")))
(use-package epic
  :commands (epic-create-note-from-region
  epic-insert-selected-note-as-org-links
  epic-create-note-from-org-buffer)
  :config
  (setq epic-evernote-mail-address "tungd.5740caa@m.evernote.com"))

;; Configuration group: org
(use-package org
  :config
  (progn
    (setq org-directory "~/Documents/"
          org-default-notes-file (expand-file-name "inbox.org" org-directory))

    (setq org-capture-templates
          '(("t" "To-do" entry
             (file+headline "" "Inbox")
             "* TODO %u %?"
             :clock-keep t :kill-buffer t)
            ("r" "Links to read" checkitem
             (file+headline "" "Reading list")
             "[ ] %?"
             :clock-keep t :kill-buffer t)
            ("l" "Download" checkitem
             (file+headline "" "Download")
             "[ ] %?"
             :clock-keep t :kill-buffer t)))

    (setq org-goto-interface 'outline-path-completion
          org-log-done 'time
          org-log-into-drawer t
          org-refile-allow-creating-parent-nodes 'confirm
          org-refile-use-outline-path t
          org-return-follows-link t
          org-catch-invisible-edits 'show-and-error)

    (setq org-todo-keywords
          '((sequence "TODO(t)" "STARTED(s!)" "WAITING(w@/!)"
                      "|" "CANCELED(c@)" "DONE(d!)")))

    (setq org-src-fontify-natively t
          org-src-tab-acts-natively t)

    (setq org-hide-leading-stars t)))
(use-package org-agenda
  :commands (org-agenda org-agenda-list)
  :config
  (setq org-agenda-files
        '("~/Documents/inbox.org" "~/Documents/projects.org" "~/Documents/archives.org")
        org-agenda-skip-unavailable-files t
        org-agenda-skip-deadline-if-done nil
        org-agenda-skip-scheduled-if-done nil
        org-agenda-restore-windows-after-quit t
        org-agenda-window-setup 'current-window
        org-agenda-show-all-dates t
        org-agenda-show-log t))
(setq org-agenda-custom-commands
      '(("d" "Agenda + Next Actions" ((agenda) (todo "TODO")))
        ("w" todo "WAITING" nil)
        ("n" todo "TODO" nil)
        ("r" "Weekly Review"
         ((agenda "" ((org-agenda-ndays 7)))
          ;; type "l" in the agenda to review logged items
          (stuck "")
          (todo "PROJECT")
          (todo "MAYBE")
          (todo "WAITING")))))

(defun td/jump-to-org-agenda ()
  (interactive)
  (let ((buf (get-buffer "*Org Agenda*"))
        wind)
    (if buf
        (if (setq wind (get-buffer-window buf))
            (select-window wind)
          (if (called-interactively-p)
              (progn
                (select-window (display-buffer buf t t))
                (org-fit-window-to-buffer))
            (with-selected-window (display-buffer buf)
              (org-fit-window-to-buffer))))
      (call-interactively 'org-agenda-list))))

(run-with-idle-timer 2400 t 'td/jump-to-org-agenda)
(setq org-tag-alist '(("@work" . ?W)     ; Contexts
                      ("@home" . ?H)
                      ("@school" . ?S)
                      ("@errand" . ?E)
                      ("build" . ?b)     ; Task types
                      ("earn" . ?e)
                      ("learn" . ?l)
                      ("focus" . ?f)     ; Task statuses
                      ("someday" . ?s)
                      ("delegate" . ?d)))
(setq org-hide-emphasis-markers t
      org-export-with-section-numbers nil
      org-export-backends '(html latex md))
(setq org-html-preamble nil
      org-html-postamble nil
      org-html-head-include-default-style nil
      org-html-head-include-scripts nil
      org-html-head
      (concat "<link rel=\"stylesheet\" type=\"text/css\" href=\"org.css\" />\n"
              "<meta name=\"viewport\" content=\"initial-scale=1,maximum-scale=1\" />")
      org-html-text-markup-alist
      '((bold . "<strong>%s</strong>")
        (code . "<code>%s</code>")
        (italic . "<em>%s</em>")
        (strike-through . "<del>%s</del>")
        (underline . "<dfn>%s</dfn>") ; Somewhat arbitrary
        (verbatim . "<kbd>%s</kbd>")))
(use-package ob-core
  :defer t
  :config
  (progn
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((calc . t)
       (http . t)
       (python . t)
       (sql . t)
       (sqlite . t)
       (elasticsearch . t)))

    (defun td/org-babel-whitelist (lang body)
      (not (or (string= lang "http")
               (string= lang "es"))))

    (setq org-confirm-babel-evaluate #'td/org-babel-whitelist)))
(bind-key "C-c o a" #'org-agenda)
(bind-key "C-c o c" #'org-capture)
(bind-key "C-c o l" #'org-store-link)

;; Configuration group: init-after
(defun td/show-startup-time ()
  "Show Emacs's startup time in the minibuffer"
  (message "Startup time: %s seconds."
           (emacs-uptime "%s")))

(add-hook 'emacs-startup-hook 'td/show-startup-time 'append)
(use-package server
  :defer 1
  :commands server-running-p
  :config
  (progn
    (unless (server-running-p) (server-start))))
(setq backup-directory-alist
      `((".*" . ,td/data-directory)))
(setq auto-save-list-file-prefix td/data-directory
      auto-save-timeout (* 5 60))
(setq create-lockfiles nil)
(setq user-full-name "Tung Dao"
      user-mail-address "me@tungdao.com")

;; emacs.el ends here
