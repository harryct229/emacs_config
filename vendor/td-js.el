
;; Switched to js2-mode
(td/after 'js
  (require 'js-indentation)

  (setq-default js-indent-level 2
                js-expr-indent-offset 2
                js-flat-functions t)

  (td/on 'js-mode-hook (tern-mode t)))


;; Use my own vendored js-comint
(td/after 'js-comint
  ;; TODO: this package need serious love
  (setenv "NODE_NO_READLINE" "1")
  (setq inferior-js-program-command "node")

  (defun td/setup-inferior-js ()
    (ansi-color-for-comint-mode-on))

  (add-hook 'inferior-js-mode-hook #'td/setup-inferior-js))