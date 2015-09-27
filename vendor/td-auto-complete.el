
;;;; auto-complete
(td/after 'auto-complete
  (setq ac-auto-start nil
        ac-disable-inline t
        ac-expand-on-auto-complete nil
        ac-ignore-case nil
        ac-use-menu-map t)

  (ac-set-trigger-key "TAB")

  (ac-linum-workaround)
  (ac-flyspell-workaround)

  (add-to-list 'ac-modes 'scss-mode)
  (add-to-list 'ac-modes 'html-mode)
  (add-to-list 'ac-modes 'coffee-mode)
  (add-to-list 'ac-modes 'typescript-mode)
  (add-to-list 'ac-modes 'cider-mode)
  (add-to-list 'ac-modes 'nodejs-repl-mode)

  (defun current-buffer-line-candidates ()
    (-uniq (mapcar #'s-trim (current-buffer-lines))))

  (ac-define-source buffer-lines
    '((prefix . "^\s*\\(.+\\)")
      (candidates . current-buffer-line-candidates)))

  (td/bind td/completion-map
           "s" #'ac-complete-yasnippet
           "f" #'ac-complete-filename
           "l" #'ac-complete-buffer-lines
           "h" #'ac-quick-help
           "t" #'ac-complete-tern-completion)

  (td/bind ac-menu-map
           "C-n" #'ac-next
           "C-p" #'ac-previous
           "C-l" #'ac-expand-common))

(td/after 'auto-complete-config
  ;; (ac-config-default)
  (setq-default ac-sources '(ac-source-yasnippet
                             ac-source-imenu
                             ac-source-words-in-same-mode-buffers
                             ac-source-dictionary))

  (require 'ac-c-headers)

  (defvar td/local-ac-sources
    '((emacs-lisp-mode . (ac-source-symbols
                          ac-source-functions
                          ac-source-variables
                          ac-source-features))
      (css-mode . (ac-source-css-property))
      (scss-mode . (ac-source-css-property))
      (js-mode . (ac-source-tern-completion))
      (c-mode . (ac-source-c-headers
                 ac-source-c-header-symbols))))

  (defun td/set-local-ac-sources ()
    (let* ((sources (cdr (assoc major-mode td/local-ac-sources)))
           (prefixes '(ac-source-yasnippet
                       ac-source-imenu
                       ac-source-words-in-same-mode-buffers))
           (suffixes '(ac-source-dictionary))
           (local-sources (append prefixes sources suffixes)))
      (when sources
        (td/set-local 'ac-sources local-sources))))

  (add-hook 'after-change-major-mode-hook #'td/set-local-ac-sources))

(td/after 'auto-complete-autoloads
  (require 'auto-complete-config)
  (global-auto-complete-mode t))
