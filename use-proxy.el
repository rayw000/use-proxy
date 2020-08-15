;;; -*- lexical-binding: t; -*-
;;; use-proxy.el --- Use proxy globally or limiting to single s-expression.

;;; Copyright ()
(require 'exec-path-from-shell)

(defgroup use-proxy nil
  "Use proxy globally or limiting to single S-expression."
  :prefix "use-proxy-")

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)
  (dolist (env '("HTTP_PROXY" "HTTPS_PROXY" "SOCKS" "NO_PROXY"))
    (exec-path-from-shell-copy-env env)))

(defcustom use-proxy-http-proxy (getenv "HTTP_PROXY")
  "HTTP proxy in HOST:PORT format, default is the value of $HTTP_PROXY"
  :type '(string)
  :group 'use-proxy)

(defcustom use-proxy-https-proxy (or (getenv "HTTPS_PROXY")
                                     (getenv "HTTP_PROXY"))
  "HTTPS proxy in HOST:PORT format. If not set, it will first try to
use the value of $HTTPS_PROXY, and then $HTTP_PROXY"
  :type '(string)
  :group 'use-proxy)

(defcustom use-proxy-no-proxy (or (getenv "NO_PROXY")
                                  "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
  "Regular expression described hosts you don't want to access through a proxy.
 If not set, it will first try to use the value of $NO_PROXY,
 and then \"^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)\""
  :type '(string)
  :group 'use-proxy)

(defvar use-proxy--available-protocols '("http" "https"))

(define-minor-mode use-proxy-mode
  "Toggle proxy mode."
  :init-value nil
  :lighter (:eval (concat "proxy["
                          (string-join
                           (mapcar #'car
                                   (seq-filter (lambda (x) (not (string= (car x) "no_proxy")))
                                               url-proxy-services)) ",")
                          "]"
                          (when (eq nil (assoc "no_proxy" url-proxy-services))
                            "g")))
  :group 'use-proxy
  :global t)

(defun use-proxy--trim-proxy-address (address)
  "Trim proxy address from <scheme>://<host>:<port> into <host>:<port>
 because the former may lead name resolving errors. Default bound to [C-c -C-p]"
  (car (last (split-string address "//"))))

(defun use-proxy--get-proxy-by-proto (proto)
  "Get proxy setting by protocol."
  (use-proxy--trim-proxy-address
   (symbol-value (intern-soft (format "use-proxy-%s-proxy" proto)))))

(defun use-proxy-toggle-global ()
  "Toggle if use proxies globally by set/unset no_proxy key in `url-proxy-services'."
  (interactive)
  (let ((no-proxy use-proxy-no-proxy))
    (if (eq nil (assoc "no_proxy" url-proxy-services))
        (add-to-list 'url-proxy-services `("no_proxy" . ,no-proxy))
      (setq url-proxy-services
            (assoc-delete-all "no_proxy" url-proxy-services)))))

(defun use-proxy-toggle-proxy (proto)
  "Toggle proxy on/off. You can switch proxy per protocol,
and proxy status will show on mode-line. This function will set/unset
`url-proxy-services' to enable/disable proxies."
  (interactive "P")
  (let* ((proto (completing-read
                 "Switch proxy for: "
                 use-proxy--available-protocols
                 nil t ""))
         (proxy (use-proxy--get-proxy-by-proto proto)))
    (if (eq nil (assoc proto url-proxy-services))
        (add-to-list 'url-proxy-services `(,proto . ,proxy))
      (setq url-proxy-services
            (assoc-delete-all proto url-proxy-services)))))

(defmacro use-proxy-with-custom-proxies (protos &rest body)
  "Use proxies on a group of S-expressions. This function respects `use-proxy-<protocol>-proxy'
variables and provide a local `url-proxy-services' to argument `body'."
  `(let ((url-proxy-services
          (mapcar (lambda (proto)
                    (cons proto (use-proxy--get-proxy-by-protocol proto)))
                  ,protos)))
     ,@body))

(defmacro use-proxy-with-specified-proxies (protos-assoc &rest body)
  "Use proxies on a group of S-expressions. This function doesn't respect your custom
`use-proxy-<protocol>-proxy' variables. It provides a local `url-proxy-services'
 to argument `body'."
  `(let ((url-proxy-services ,protos-assoc))
     ,@body))

(provide 'use-proxy)
;;; use-proxy.el ends here
