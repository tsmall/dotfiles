(require 'package)

(setq package-archives-additional
      '(("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))

(dolist (archive package-archives-additional)
  (add-to-list 'package-archives archive))

(setq package-list
      '(ace-jump-mode
        cider
        clojure-mode
        crystal-mode
        csv-mode
        editorconfig
        elixir-mode
        elm-mode
        erlang
        exec-path-from-shell
        expand-region
        fiplr
        full-ack
        go-mode
        haskell-mode
        highlight-symbol
        iy-go-to-char
        js2-mode
        json-mode
        julia-mode
        lua-mode
        magit
        markdown-mode
        multiple-cursors
        olivetti
        paredit
        parinfer
        perl6-mode
        php-mode
        rainbow-mode
        swift-mode
        tuareg
        web-mode
        yaml-mode
        yasnippet))

;; activate all the packages
(package-initialize)

;; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
