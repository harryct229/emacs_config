
(defadvice js2-proper-indentation (after js2-my-indentation activate)
  ;; Continued expression
  (when (and (js2-continued-expression-p)
             (js2-multiline-decl-indentation))
    (setq ad-return-value (- ad-return-value (* 2 js2-basic-offset))))

  ;; Continued argument list
  ;; This is extremly similar to which of `js-mode', and surprising enough,
  ;; Steve Yegge was using `js-mode' indentation code as inspiration.
  (when (nth 1 parse-status)
    (save-excursion
      (goto-char (nth 1 parse-status))
      (unless (looking-at "[({[]\\s-*\\(/[/*]\\|$\\)")
        ;; argslist-cont
        (back-to-indentation)
        (setq ad-return-value (+ js2-basic-offset (current-column))))))

  ;; Leading comma - a.k.a npm style
  (save-excursion
    (back-to-indentation)
    (when (looking-at ",")
      (setq ad-return-value (+ ad-return-value js2-basic-offset)))))

;; (ad-deactivate 'js2-proper-indentation)


(defadvice js--proper-indentation (after js-my-indentation activate)
  ;; Leading comma style
  (save-excursion
    (back-to-indentation)
    (if (looking-at ",")
        (setq ad-return-value (+ ad-return-value js-expr-indent-offset))))
  ;; Bracket related
  (when (nth 1 parse-status)
    (save-excursion
      (let ((continued-expr-p (js--continued-expression-p)))
        (goto-char (nth 1 parse-status))
        (if (looking-at "[({[]\\s-*\\(/[/*]\\|$\\)")
            ;; Continued expression
            (when continued-expr-p
              (skip-syntax-backward " ")
              (when (eq (char-before) ?\)) (backward-list))
              (back-to-indentation)
              (setq ad-return-value
                    (+ (current-column) js-indent-level js-expr-indent-offset)))
          ;; argslist-cont
          (setq ad-return-value
                (+ js-indent-level js-expr-indent-offset)))))))

;; (ad-deactivate 'js--proper-indentation)


(provide 'js-indentation)
