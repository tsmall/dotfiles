;;; el-get-settings.el --- Settings for el-get

;;; Commentary:

;; This file ensures el-get is installed, sets its configuration settings,
;; and synchronizes all of the packages.

;;; Code:

;;; Initialization:
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;;; Settings:
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(setq el-get-user-package-directory "~/.emacs.d/el-get-init-files")

;;; Packages:
(setq el-get-packages 
      '(ace-jump-mode
        caml-mode
        cider
        clojure-mode
        csv-mode
        editorconfig
        exec-path-from-shell
        expand-region
        fiplr
        full-ack
        haskell-mode
        highlight-symbol
        iy-go-to-char
        js2-mode
        json-mode
        lua-mode
        magit
        markdown-mode
        multiple-cursors
        paredit
        php-mode
        rainbow-mode
        rjsx-mode
        swift-mode
        web-mode
        yaml-mode
        yasnippet))

(el-get 'sync el-get-packages)

(provide 'el-get-settings)

;;; el-get-settings.el ends here
