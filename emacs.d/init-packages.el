(require 'package)

(setq package-archives-additional
      '(("melpa" . "https://melpa.org/packages/")
	    ;; ("marmalade" . "https://marmalade-repo.org/packages/")
        ))

(dolist (archive package-archives-additional)
  (add-to-list 'package-archives archive))

(setq package-list
      '(ace-jump-mode
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
        fiplr
        fsharp-mode
        full-ack
        go-mode
        haskell-mode
        highlight-symbol
        iy-go-to-char
        janet-mode
        js2-mode
        json-mode
        julia-mode
        lua-mode
        magit
        markdown-mode
        move-text
        multiple-cursors
        olivetti
        paredit
        parinfer
        php-mode
        rainbow-mode
        raku-mode
        swift-mode
        tide
        tuareg
        typescript-mode
        vala-mode
        vimish-fold
        web-mode
        yaml-mode
        yasnippet
        zig-mode))

;; activate all the packages
(package-initialize)

;; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
