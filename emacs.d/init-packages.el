(require 'package)

(setq package-archives-additional
      '(("melpa" . "https://melpa.org/packages/")
	    ;; ("marmalade" . "https://marmalade-repo.org/packages/")
        ))

(dolist (archive package-archives-additional)
  (add-to-list 'package-archives archive))

(setq package-list
      '(ace-jump-mode
        adaptive-wrap
        bongo
        cider
        clojure-mode
        crystal-mode
        csv-mode
        editorconfig
        elfeed
        elixir-mode
        elm-mode
        elpher
        erlang
        exec-path-from-shell
        expand-region
        feature-mode
        fsharp-mode
        full-ack
        go-mode
        haskell-mode
        helm-rg
        highlight-symbol
        iy-go-to-char
        janet-mode
        js2-mode
        json-mode
        julia-mode
        kotlin-mode
        ledger-mode
        lua-mode
        magit
        markdown-mode
        move-text
        multiple-cursors
        olivetti
        paredit
        parinfer
        php-mode
        projectile
        rainbow-mode
        raku-mode
        rg
        swift-mode
        tide
        treemacs
        tuareg
        typescript-mode
        vala-mode
        vimish-fold
        web-mode
        yaml-mode
        yafolding
        yasnippet
        zig-mode))

;; activate all the packages
(package-initialize)

;; fetch the list of packages available 
(package-refresh-contents)

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
