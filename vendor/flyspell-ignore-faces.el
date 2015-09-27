
;;; This seems like a cool idea, except overlays kills everything.

(require 'dash)

(defvar fsif/flyspell-ignored-faces nil
  "List of faces that should not be spell checked.")

(add-to-list 'fsif/flyspell-ignored-faces 'org-block-meta-line)
(add-to-list 'fsif/flyspell-ignored-faces 'org-block-begin-line)
(add-to-list 'fsif/flyspell-ignored-faces 'org-block-end-line)
(add-to-list 'fsif/flyspell-ignored-faces 'org-block-background)
(add-to-list 'fsif/flyspell-ignored-faces 'org-verbatim)
(add-to-list 'fsif/flyspell-ignored-faces 'org-formula)

(defun fsif/face-at-point ()
  "Return the current face under cursor."
  (or (get-char-property (point) 'read-face-name)
      (get-char-property (point) 'face)))

(defun fsif/flyspell-ignored-face-p (face)
  "Whether the face should be ignored."
  (member face fsif/flyspell-ignored-faces))

(defun fsif/flyspell-check-p ()
  "Function used for `flyspell-generic-check-word-predicate'
        to ignore text with faces defined in `fsif/flyspell-ignored-faces'."
  (let ((faces (fsif/face-at-point)))
    (or (not faces)
        (not (if (listp faces)
                 (-any-p #'fsif/flyspell-ignored-face-p faces)
               (fsif/flyspell-ignored-face-p faces))))))

(provide 'flyspell-ignore-faces)
