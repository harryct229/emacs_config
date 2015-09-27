
(defun company-filename--paths (&optional prefix)
  (let* ((path (thing-at-point 'filename))
         (dir (or (file-name-directory path) "")))
    (directory-files dir)))

;;;###autoload
(defun company-filename (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-filename))
    (prefix (file-name-base (thing-at-point 'filename)))
    (candidates (all-completions arg (company-filename--paths)))
    (sorted t)))

(provide 'company-filename)
