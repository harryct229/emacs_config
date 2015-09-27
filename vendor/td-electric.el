
;;;; electric
(electric-pair-mode t)
(electric-indent-mode t)

(defun td/smart-brace ()
  (when (and (eq last-command-event ?\n)
             (looking-at "[<}]"))
    (indent-according-to-mode)
    (forward-line -1)
    (end-of-line)
    (newline-and-indent)))

(defun td/smart-parenthesis ()
  (when (and (eq last-command-event ?\s)
             (or (and (looking-back "( " (- (point) 2))
                      (looking-at ")"))
                 (and (looking-back "{ " (- (point) 2))
                      (looking-at "}"))
                 (and (looking-back "\\[ " (- (point) 2))
                      (looking-at "\\]"))))
    (insert " ")
    (backward-char 1)))

(add-hook 'post-self-insert-hook #'td/smart-brace t)
(add-hook 'post-self-insert-hook #'td/smart-parenthesis t)

(provide 'td-electric)
