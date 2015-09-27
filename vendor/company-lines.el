
(require 's)

(defun company-lines--buffer-lines (&optional buffer)
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (mapcar #'s-trim
              (split-string (buffer-string) "[\n\r]")))))

;;;###autoload
(defun company-lines (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-lines))
    (prefix (when (looking-back "^\s*\\(.+\\)")
              (match-string-no-properties 1)))
    (candidates (all-completions arg (company-lines--buffer-lines)))
    (sorted t)))

(provide 'company-lines)
