;;; process-attributes.el --- Lisp implementation of `process-attributes'

;; Copyright (c) 2014 Tung Dao

;; Author: Łukasz Gruner <lukasz@gruner.lu>
;; Maintainer: Łukasz Gruner <lukasz@gruner.lu>
;; Version: 20141107.1517
;; X-Original-Version: 2
;; Package-Requires: ((emacs "24.4"))
;; URL: https://bitbucket.org/ukaszg/aria2-mode
;; Created: 19/10/2014
;; Keywords: download bittorrent aria2

;; This file is not part of Emacs.


;; This work ‘as-is’ we provide.
;; No warranty express or implied.
;; We’ve done our best,
;; to debug and test.
;; Liability for damages denied.
;;
;; Permission is granted hereby,
;; to copy, share, and modify.
;; Use as is fit,
;; free or for profit.
;; These rights, on this notice, rely.


;;; Commentary:

;;; Code:

(defvar pa--fields nil
  "Mapping from expected return value and `ps' keyworkds.
Note that since comm and args are truncated in the ps result, it
needs a separate ps call.")

(setq pa--fields
      '((euid . (uid string-to-int))        ;;- Effective user User ID of the process (number)
        (user . (user identity))       ;;- User name corresponding to euid (string)
        (egid . (gid string-to-int))        ;;- Effective user Group ID of the process (number)
        (group . (group identity))     ;;- Group name corresponding to egid (string)
        (comm . (comm pa--comm))       ;;- Command name (executable name only) (string)
        (state . (state identity))     ;;- Process state code, such as "S", "R", or "T" (string)
        (ppid . (ppid string-to-int))       ;;- Parent process ID (number)
        (pgrp . (pgrp string-to-int))       ;;- Process group ID (number)
        (sess . (sess string-to-int))       ;;- Session ID, i.e. process ID of session leader (number)
        (ttname . (tty identity))      ;;- Controlling tty name (string)
        (tpgid . (tpgid string-to-int))     ;;- ID of foreground process group on the process's tty (number)
        (minflt . (minflt string-to-int))   ;;- number of minor page faults (number)
        (majflt . (majflt string-to-int))   ;;- number of major page faults (number)
        ;; (cminflt . cminflt) ;;- cumulative number of minor page faults (number)
        ;; (cmajflt . cmajflt) ;;- cumulative number of major page faults (number)
        (utime . (utime pa--string-to-time))     ;;- user time used by the process, in (current-time) format which is a list of integers (HIGH LOW USEC PSEC)
        ;; (stime . stime)     ;;- system time used by the process (current-time)
        (time . (time pa--string-to-time))       ;;- sum of utime and stime (current-time)
        ;; (cutime . cutime)   ;;- user time used by the process and its children (current-time)
        ;; (cstime . cstime)   ;;- system time used by the process and its children (current-time)
        ;; (ctime . ctime)     ;;- sum of cutime and cstime (current-time)
        (pri . (pri string-to-int))         ;;- priority of the process (number)
        (nice . (nice string-to-int))       ;;- nice value of the process (number)
        (thcount . (wq string-to-int))      ;;- process thread count (number)
        (start . (start pa--start))     ;;- time the process started (current-time)
        (vsize . (vsize string-to-int))     ;;- virtual memory size of the process in KB's (number)
        (rss . (rss string-to-int))         ;;- resident set size of the process in KB's (number)
        (etime . (etime pa--string-to-time))     ;;- elapsed time the process is running, in (HIGH LOW USEC PSEC) format
        (pcpu . (pcpu string-to-number))       ;;- percents of CPU time used by the process (floating-point number)
        (pmem . (pmem string-to-number))       ;;- percents of total physical memory used by process's resident set (floating-point number)
        (args . (args pa--args))       ;;- command line which invoked the process (string).
        ))

(defun pa--args (e)
  e)

(defun pa--comm (e)
  e)

(defun pa--string-to-time (e)
  e)

(when (eq system-type 'darwin)
  (defun process-attributes (pid)
    (let* ((cmd (pa--query-cmd pid))
           (data (shell-command-to-string (pa--query-cmd 11968)))
           (data (split-string data "\n"))
           (headers (split-string (cadr data)))
           (values (split-string (car data)))
           (attrs (list)))
      (while headers
        (let ((field (pop headers))
              (value (pop values)))
          (push (cons (intern value) field) attrs)))
      attrs)))

(process-attributes 11968)

(defvar pa--template "ps -o %s %d")

(defun pa--query-cmd (pid)
  (format pa--template (pa--field-flags) pid))



(defun pa--field-flags ()
  (string-join
   (mapcar (lambda (e)
             (format "%s=%s" (cadr e) (car e)))
           pa--fields)
   ","))

(pa--field-flags)

(provide 'process-attributes)
