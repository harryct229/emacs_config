
(require 'ido)

(defun ido-sort-mtime--sort (a b)
  (let ((a-path (concat ido-current-directory b))
        (b-path (concat ido-current-directory a))
        (a-tramp-file-p (string-match-p ":" a))
        (b-tramp-file-p (string-match-p ":" b)))
    (cond
     ((and a-tramp-file-p b-tramp-file-p)
      (string< a b))
     ((or a-tramp-file-p (not (file-readable-p a-path)))
      nil)
     ((or b-tramp-file-p (not (file-readable-p b-path)))
      t)
     (t (time-less-p
         (nth 5 (file-attributes a-path))
         (nth 5 (file-attributes b-path)))))))

(defun ido-sort-mtime--dot-filep (f)
  (and (char-equal (string-to-char f) ?.) f))

(defun ido-sort-mtime ()
  (setq ido-temp-list
        (sort ido-temp-list #'ido-sort-mtime--sort))
  (ido-to-end  ;; move . files to end (again)
   (delq nil (mapcar #'ido-sort-mtime--dot-filep ido-temp-list))))

(add-hook 'ido-make-file-list-hook #'ido-sort-mtime)
(add-hook 'ido-make-dir-list-hook #'ido-sort-mtime)

(provide 'ido-sort-mtime)
