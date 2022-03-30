(require 'package)
(push '("melpa" . "https://melpa.org/packages/") package-archives)
(package-initialize)
(dolist (pkg '(exec-path-from-shell))
  (unless (package-installed-p pkg)
    (unless (assoc pkg package-archive-contents)
      (package-refresh-contents))
    (package-install pkg)))

(add-to-list 'load-path (concat (file-name-directory load-file-name)))

(require 'ert)
(require 'use-proxy)

(ert-deftest use-proxy-test-toggle-proxies-global ()
  (dolist (repeat '(on off))
    (if (assoc "no_proxy" url-proxy-services)
        (progn
          (use-proxy-toggle-proxies-global)
          (should (equal nil (assoc "no_proxy" url-proxy-services))))
      (progn
        (use-proxy-toggle-proxies-global)
        (should (not (equal nil (assoc "no_proxy" url-proxy-services))))))))

(ert-deftest use-proxy-test-toggle-proto-proxy ()
  (dolist (proto use-proxy--available-protocols)
    (dolist (repeat '(on off))
      (if (equal nil (assoc proto url-proxy-services))
          (progn
            (use-proxy-toggle-proto-proxy proto)
            (should (equal (cons proto (use-proxy--get-custom-proxy-var-by-proto proto))
                           (assoc proto url-proxy-services))))
        (progn
          (use-proxy-toggle-proto-proxy proto)
          (should (equal nil (assoc proto url-proxy-services))))))))

(ert-deftest use-proxy-test-toggle-all-proxies ()
  (dolist (repeat '(on off))
    (if (equal nil url-proxy-services)
        (progn
          (use-proxy-toggle-all-proxies)
          (dolist (proto use-proxy--available-protocols)
            (should (equal (cons proto (use-proxy--get-custom-proxy-var-by-proto proto))
                           (assoc proto url-proxy-services))))
          (should (equal (cons "no_proxy" use-proxy-no-proxy)
                         (assoc "no_proxy" url-proxy-services)))))
    (if (not (equal nil url-proxy-services))
        (progn
          (use-proxy-toggle-all-proxies)
          (should (equal nil url-proxy-services))))))

(ert-deftest use-proxy-test-with-custom-proxies ()
  (use-proxy-with-custom-proxies use-proxy--available-protocols
                                 (dolist (proto use-proxy--available-protocols)
                                   (should (equal (use-proxy--get-custom-proxy-var-by-proto proto)
                                                  (cdr (assoc proto url-proxy-services)))))))

(ert-deftest use-proxy-test-with-specified-proxies ()
  (let ((protos-assoc '(("http" . "localhost:8001")
                        ("https" . "localhost:8002"))))
    (should (equal protos-assoc
                   (use-proxy-with-specified-proxies protos-assoc url-proxy-services)))))
