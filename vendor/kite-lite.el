;;; kite-lite.el --- remote evaluate JavaScript in  WebKit debugger
;;
;; Copyright (c) 2014  Tung Dao <me@tungdao.com>
;;
;; Author: Tung Dao <me@tungdao.com>
;; URL: https://github.com/tungd/kite-lite.el
;; Keywords: webkit
;; Version: 0.1.0
;; Package-Requires: ((dash "1.5.0") (websocket "1.2"))
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Minor mode for remote evaluate JavaScript in WebKit debugger, with
;; a little icing. Included features are:
;; - Evaluate JavaScript (running at top level)
;; - Modify and update external JavaScript files (live)
;; - Reload
;;
;; Planned features includes:
;; - Live reload stylesheets (without reload)
;; - JavaScript console (REPL)
;;
;;; Code:

(require 'url)
(require 'json)
(require 'dash)
(require 'websocket)


(defcustom kl-remote-host "127.0.0.1"
  "Default host for connection to WebKit remote debugging API."
  :group 'kite-lite)

(defcustom kl-remote-port 9222
  "Default port for connection to WebKit remote debugging API."
  :group 'kite-lite)

(defvar kl-socket nil
  "Websocket connection to WebKit remote debugging API.")

(defvar kl-rpc-id 0)
(defvar kl-rpc-callbacks nil)
(defvar kl-rpc-scripts nil
  "List of JavaScript files available for live editing.")


(defun kl-encode (data)
  (let ((json-array-type 'list)
        (json-object-type 'plist))
    (json-encode data)))

(defun kl-decode (data)
  (let ((json-array-type 'list)
        (json-object-type 'plist))
    (json-read-from-string data)))

(defun kl-next-rpc-id ()
  (setq kl-rpc-id (+ 1 kl-rpc-id)))


(defun kl-register-callback (id fn)
  (let ((hook (intern (number-to-string id) kl-rpc-callbacks)))
    (add-hook hook fn t)))

(defun kl-dispatch-callback (id data)
  (let ((hook (intern (number-to-string id) kl-rpc-callbacks)))
    (when hook
      (run-hook-with-args hook data)
      (unintern hook kl-rpc-callbacks))))


(defun kl-on-open (socket)
  (message "Kite: connected."))

(defun kl-on-close (socket)
  (message "Kite: disconnected."))

(defun kl-on-script-parsed (data)
  (let ((extension? (plist-get data :isContentScript))
        (url (plist-get data :url))
        (id (plist-get data :scriptId)))
    (when (and url (not extension?))
      (add-to-list 'kl-rpc-scripts (list :id id :url url)))))

(defun kl-on-message-added (data)
  "We got `source', `level', `text' and the possition that trigger it."
  (let* ((message (plist-get data :message))
         (type (plist-get message :type))
         (text (plist-get message :text)))
    (message "Kite: [%s] %s" type text)))

(defun kl-on-message (socket data)
  (let* ((data (kl-decode (websocket-frame-payload data)))
         (method (plist-get data :method))
         (params (plist-get data :params)))
    (cond
     ((string-equal method "Debugger.scriptParsed")
      (kl-on-script-parsed params))
     ((string-equal method "Console.messageAdded")
      (kl-on-message-added params))
     ;; TODO: do something usefull here, possibly great for REPL
     ((string-equal method "Console.messageRepeatCountUpdated"))
     ;; These are return messages from RPC calls, not notification
     ((not method)
      (kl-dispatch-callback (plist-get data :id) (plist-get data :result)))
     ;; Generic fallback, only used in development
     (t (message "Kite: %s" data)))))

(defun kl-call-rpc (method &optional params)
  (websocket-send-text
   kl-socket
   (kl-encode (list :id (kl-next-rpc-id)
                    :method method
                    :params params))))

(defun kl-open-socket (url)
  (websocket-open socket-url
                  :on-open #'kl-on-open
                  :on-message #'kl-on-message
                  :on-close #'kl-on-close))


(defun kl-get-json (url)
  (let* ((url-request-method "GET")
         (url-http-attempt-keepalives nil)
         (json-array-type 'list)
         (json-object-type 'plist))
    (with-current-buffer (url-retrieve-synchronously url)
      (if (not (eq 200 (url-http-parse-response)))
          (error "Unable to connect to host.")
        (goto-char (+ 1 url-http-end-of-headers))
        (json-read)))))

(defun kl-get-tabs (host port)
  (let* ((url (url-parse-make-urlobj
               "http" nil nil host port "/json"))
         (tabs (kl-get-json url)))
    (-filter (lambda (tab)
               (and (plist-get tab :webSocketDebuggerUrl)
                    (string-equal (plist-get tab :type) "page")))
             tabs)))

(defun kl-tab-completion (tab)
  (let ((title (plist-get tab :title))
        (url (plist-get tab :url)))
    (cons (format "%s" title) tab)))

(defun kl-select-tab (host port)
  (let* ((tabs (mapcar #'kl-tab-completion
                       (kl-get-tabs host port)))
         (selection (completing-read
                     "Tab: " tabs nil t "" nil (caar tabs)))
         (tab (cdr (assoc selection tabs))))
    (plist-get tab :webSocketDebuggerUrl)))


(defun kl-connect ()
  (interactive)
  (kl-disconnect)
  (let* ((socket-url (kl-select-tab kl-remote-host
                                    kl-remote-port)))
    (setq kl-socket (kl-open-socket socket-url))
    (kl-call-rpc "Console.enable")
    (kl-call-rpc "Debugger.enable")
    (kl-call-rpc "Network.setCacheDisabled" '(:cacheDisabled t))))

(defun kl-disconnect ()
  (interactive)
  (when (websocket-openp kl-socket)
    (websocket-close kl-socket)
    (setq kl-socket nil
          kl-rpc-scripts nil)))


(defun kl-send-eval (code)
  (kl-call-rpc
   "Runtime.evaluate"
   (list :expression code
         :returnByValue t)))

(defun kl-remove-script (script)
  (setq kl-rpc-scripts
        (delete script kl-rpc-scripts)))

(defun kl-script-id (file)
  (let ((result nil)
        (name (file-name-base file)))
    (dolist (script kl-rpc-scripts result)
      (let ((id (plist-get script :id))
            (url (plist-get script :url)))
        (when (string-equal name (file-name-base url))
          (if (not (kl-script-exists? id))
            (kl-remove-script script)
            (setq id (plist-get script :id))))))))

(defun kl-update ()
  (interactive)
  (let ((id (kl-script-id (buffer-file-name)))
        (source (buffer-substring-no-properties
                 (point-min) (point-max))))
    (if id
        (kl-call-rpc
         "Debugger.setScriptSource"
         (list :scriptId id :scriptSource source))
      (message "No matching script for current buffer."))))

(defun kl-reload ()
  (interactive)
  (kl-call-rpc
   "Page.reload"
   (list :ignoreCache t)))

(defun kl-evaluate-region-or-line (&optional args)
  (interactive "*P")
  (let ((start (if (region-active-p)
                   (region-beginning)
                 (line-beginning-position)))
        (end (if (region-active-p)
                 (region-end)
               (line-end-position))))
    (kl-send-eval (buffer-substring-no-properties start end))))


(defvar kite-lite-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-e") #'kl-evaluate-region-or-line)
    (define-key map (kbd "C-x C-u") #'kl-update)
    (define-key map (kbd "C-x C-r") #'kl-reload))
  "Keymap for Kite Lite mode.")

;;;###autoload
(defun turn-on-kite-lite-mode ()
  "Turn on Kite Lite mode.")

;;;###autoload
(defun turn-off-kite-lite-mode ()
  "Turn off Kite Lite mode.")

;;;###autoload
(define-minor-mode kite-lite-mode
  "Minor mode for interact with WebKit remote debugging API."
  :global nil
  :group 'kite-lite
  :init-value nil
  :lighter ""
  :keymap kite-lite-mode-map
  (if kite-lite-mode
      (turn-on-kite-lite-mode)
    (turn-off-kite-lite-mode)))

(provide 'kite-lite)
;;; kite-lite.el ends here
