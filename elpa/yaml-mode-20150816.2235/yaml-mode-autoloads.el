;;; yaml-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (yaml-mode yaml) "yaml-mode" "yaml-mode.el" (22021
;;;;;;  61848 205096 597000))
;;; Generated autoloads from yaml-mode.el

(let ((loads (get 'yaml 'custom-loads))) (if (member '"yaml-mode" loads) nil (put 'yaml 'custom-loads (cons '"yaml-mode" loads))))

(autoload 'yaml-mode "yaml-mode" "\
Simple mode to edit YAML.

\\{yaml-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.e?ya?ml$" . yaml-mode))

;;;***

;;;### (autoloads nil nil ("yaml-mode-pkg.el") (22021 61848 294499
;;;;;;  574000))

;;;***

(provide 'yaml-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; yaml-mode-autoloads.el ends here
