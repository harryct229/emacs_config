;; Enable accents-folding for ISearch. Support only Vietnamese accents atm.
;; Source:
;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2012-12/msg00034.html

(defvar idf--standard-case-table (standard-case-table))
(defvar idf--decomposition-table nil)

;; (defun idf--decompose (c)
;;   (let ((d (get-char-code-property c 'decomposition)))
;;     (unless (characterp (car d) (pop d)))
;;     (if (eq (car d) (idf--decompose (car d)))
;;         d
;;       (idf--decompose (car d)))))

;; (defun idf--make-decomposition-table ()
;;   (let* ((table (standard-case-table))
;;          (canon (copy-sequence table))
;;          (c #x0000))
;;     (while (<= c #xFFFD)
;;       (aset canon c (idf--decompose c))
;;       (setq c (1+ c)))
;;     (set-char-table-extra-slot table 1 canon)
;;     (set-char-table-extra-slot table 2 nil)
;;     (setq idf--decomposition-table table)))


;; TODO: refactor
(defun idf--make-decomposition-table-1 (canon c0 c1)
  (let ((d (get-char-code-property c1 'decomposition)))
    (when d
      (unless (characterp (car d)) (pop d))
      (if (eq c1 (car d))
          (aset canon c0 (car d))
        (idf--make-decomposition-table-1 canon c0 (car d))))))

(defun idf--make-decomposition-table ()
  (let ((table (standard-case-table))
        canon)
    (setq canon (copy-sequence table))
    (let ((c #x0000) d)
      (while (<= c #xFFFD)
        (idf--make-decomposition-table-1 canon c c)
        (setq c (1+ c))))
    (set-char-table-extra-slot table 1 canon)
    (set-char-table-extra-slot table 2 nil)
    (setq idf--decomposition-table table)))


;;;###autoload
(defun turn-off-isearch-diacritics-fold-mode ()
  (set-case-table idf--standard-case-table))

;;;###autoload
(defun turn-on-isearch-diacritics-fold-mode ()
  (unless idf--decomposition-table
    (idf--make-decomposition-table))
  (set-case-table idf--decomposition-table))

;;;###autoload
(define-minor-mode isearch-diacritics-fold-mode
  "Toggle ISearch ignore accents."
  :global t
  :init-value nil
  :lighter ""
  :keymap nil
  (if isearch-diacritics-fold-mode
      (turn-on-isearch-diacritics-fold-mode)
    (turn-off-isearch-diacritics-fold-mode)))

(provide 'isearch-diacritics-fold)
;;; isearch-diacritics-fold.el ends here
