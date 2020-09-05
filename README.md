[![Melpa Status](http://melpa.org/packages/use-proxy-badge.svg)](https://melpa.org/#/use-proxy)
[![Melpa Stable Status](http://stable.melpa.org/packages/use-proxy-badge.svg)](http://stable.melpa.org/#/use-proxy)
[![Build Status](https://github.com/rayw000/use-proxy/workflows/test/badge.svg)](https://github.com/rayw000/use-proxy/actions)


use-proxy
====================================

### Introduction
With this package you can easily enable/disable proxies per protocol in you Emacs. You could also use this package to provide proxy settings for a group of s-expressions. This package respects your `$HTTP_PROXY`, `$HTTPS_PROXY` and `$NO_PROXY` shell environment variables.

### Install
If you don't have ELPA package in your Emacs, clone this repository and load file.
```shell
git clone https://github.com/rayw000/use-proxy.git
```
```emacs-lisp
(load-file "/path/to/use-proxy/use-proxy.el")
(require 'use-proxy)
```
### Usage
Globally enable `use-proxy-mode`.
```emacs-lisp
(use-proxy-mode)
```

You can bind keys to function `use-proxy-toggle-proto-proxy` and `use-proxy-toggle-proxies-global`, for example
```emacs-lisp
(global-set-key (kbd "C-c C-p") 'use-proxy-toggle-proto-proxy)
(global-set-key (kbd "C-c C-g") 'use-proxy-toggle-proxies-global)
```
### Customizations

This package provides these following variables you could customize:

1. `use-proxy-http-proxy`
   HTTP proxy you could use. If not set, the value of `$HTTP_PROXY` in your environment will be used.

2. `use-proxy-https-proxy`
   HTTPS proxy you could use. If not set, the value of `$HTTPS_PROXY` in your environment will be used.

3. `use-proxy-no-proxy`
   A regular expression matches hosts you don't want to connect through proxy. If not set, the value of `$NO_PROXY` in your environment will be used.

**NOTICE:** Do not forget to load your `custom-file` if you customized these variables.

### Macros and functions

1. Toggle specified proxy by protocol.

```emacs-lisp
(use-proxy-toggle-proto-proxy)
```

Running this command will prompt you available protocols to choose to enable the corresponding proxy. Enabled proxies will be shown in the minor mode lighter.

2. Toggle proxies global or not (respect `no_proxy` settings or not).

```emacs-lisp
(use-proxy-toggle-proxies-global)
```

If using proxies globally, a "g" will be appended to lighter.

3. Temporarily enable proxy for a batch of s-expressions. You are only required to provide a protocol list which you want to enable proxies for. This macro will read corresponding proxy settings from your customization variables.

```emacs-lisp
(use-proxy-with-custom-proxies '("http" "https")
  (browse-url-emacs "https://www.google.com"))
```

4. Temporarily enable proxy for a batch of s-expression. You are required to provide a proxy setting association list.

```emacs-lisp
(use-proxy-with-specified-proxies '(("http" . "localhost:8080")
                                    ("https" . "localhost:8081"))
                                  (browse-url-emacs "https://www.google.com"))
```

### How it works

All `use-proxy` does is manipulating your `url-proxy-services` variable. Every time you toggle proxies, `url-proxy-services` will be saved into your `custom.el` file. When `use-proxy-mode` on, this variable will be loaded.

### Similar packages
[with-proxy](https://github.com/twlz0ne/with-proxy.el), [proxy-mode](https://github.com/stardiviner/proxy-mode)
