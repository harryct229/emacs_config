;; tss-tools.el
;;
;; Usage:
;;
;; (defun setup-typescript ()
;;   (interactive)
;;   (add-hook 'completion-at-point-functions #'tss-completion-at-point-fn))
;;
;; (add-hook 'typescript-mode-hook #'setup-typescript)
;;
;; This works with `company-capf' out of the box, but we also have `company-tss',
;; which integrates location, signature and location.
;;

(require 'cl-lib)
(require 'tss)

(defun tss-capf--member-prefix ()
  "TODO: documentation."
  (looking-back "\\.\\([a-zA-Z0-9_]*\\)"
                (line-beginning-position)))

(defun tss-capf--type-prefix ()
  "TODO: documentation."
  (looking-back ": *\\([a-zA-Z0-9_]*\\)"
                (line-beginning-position)))

(defun tss-capf--constructor-prefix ()
  "TODO: documentation."
  (looking-back "new +\\([a-zA-Z0-9_]*\\)"
                (line-beginning-position)))

(defun tss-capf--other-prefix ()
  "TODO: documentation."
  (looking-back "\\(?:^\\|[^a-zA-Z0-9_.]\\) *\\([a-zA-Z0-9_]+\\)"
                (line-beginning-position)))

(defun tss-capf--can-complete ()
  "TODO: documentation."
  (or (tss-capf--member-prefix)
      (tss-capf--type-prefix)
      (tss-capf--constructor-prefix)
      (tss-capf--other-prefix)))

(defun tss-capf--annotation (completion)
  "TODO: documentation.")

(defun tss-completion--candidates (prefix &optional member-p)
  "TODO: documentation."
  (when (tss--sync-server)
    (let* ((cmd (if member-p "completions" "completions-brief"))
           (member (if member-p "true" "false"))
           (pos (tss--get-position-argument))
           (file (expand-file-name (buffer-file-name)))
           (ret (tss--get-server-response
                 (format "%s %s %s %s" cmd member pos file))))
      (when (listp ret)
        (cdr (assoc 'entries ret))))))

(defun tss-capf--candidates (prefix member-p)
  (let ((candidates (tss-completion--candidates prefix member-p)))
    (mapcar (lambda (c) (cdr (assoc 'name c)))
            candidates)))

;; Completion
;;;###autoload
(defun tss-completion-at-point-fn ()
  "TODO: documentation."
  (when (tss-capf--can-complete)
    (let* ((prefix (match-string-no-properties 1))
           (start (match-beginning 1))
           (end (match-end 1))
           (collection (tss-capf--candidates prefix (tss-capf--member-prefix))))
      (list start end collection))))

(defun tss-setup-capf ()
  "TODO: documentation."
  (interactive)
  (add-hook 'completion-at-point-functions #'tss-completion-at-point-fn))

(add-hook 'typescript-mode-hook #'tss-setup-capf)

;; Compilation error
(add-to-list 'compilation-error-regexp-alist
             '("^\\(.+?\\)(\\([[:digit:]]+\\),\\([[:digit:]]+\\)): \\(.*\\)$"
               1 2 3 nil 1))

;; Flycheck
(with-eval-after-load 'flycheck
  ;; TODO: find a way to check for errors using TSS similar to `tss-run-flymake'
  (flycheck-def-config-file-var flycheck-tslintrc typescript-tslint "tslint.json")

  (flycheck-define-checker tslint
    "Use tslint to flycheck TypeScript code."
    :command ("tslint"
              "--file" source
              (config-file "--config" flycheck-tslintrc)
              "--format" "prose")
    :error-patterns ((warning (file-name) "[" line ", " column "]: " (message)))
    :modes typescript-mode)

  (add-to-list 'flycheck-checkers 'tslint))

;; Company
(with-eval-after-load 'company
  (defun company-tss--candidates (prefix member-p)
    "TODO: documentation."
    (let ((candidates (tss-completion--candidates prefix member-p)))
      (mapcar (lambda (c) (cdr (assoc 'name c)))
              candidates)))

  (defun company-tss (command &optional arg &rest _)
    "TODO: documentation."
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend 'company-tss))
      (prefix (and (derived-mode-p 'typescript-mode)
                   (tss-capf--can-complete)
                   (match-string-no-properties 1)))
      (candidates (company-tss--candidates arg (tss-capf--member-prefix))))))

(provide 'tss-tools)
