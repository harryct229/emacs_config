;;; dockerfile-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (dockerfile-mode dockerfile-build-no-cache-buffer
;;;;;;  dockerfile-build-buffer) "dockerfile-mode" "dockerfile-mode.el"
;;;;;;  (22021 61782 208769 347000))
;;; Generated autoloads from dockerfile-mode.el

(autoload 'dockerfile-build-buffer "dockerfile-mode" "\
Build an image based upon the buffer

\(fn IMAGE-NAME)" t nil)

(autoload 'dockerfile-build-no-cache-buffer "dockerfile-mode" "\
Build an image based upon the buffer without cache

\(fn IMAGE-NAME)" t nil)

(autoload 'dockerfile-mode "dockerfile-mode" "\
A major mode to edit Dockerfiles.
\\{dockerfile-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("Dockerfile.*\\'" . dockerfile-mode))

;;;***

;;;### (autoloads nil nil ("dockerfile-mode-pkg.el") (22021 61782
;;;;;;  273447 253000))

;;;***

(provide 'dockerfile-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dockerfile-mode-autoloads.el ends here
