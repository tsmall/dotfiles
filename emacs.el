;; -----------------------------------------------------------------------------
;; General customizations

(setq transient-mark-mode t)           ; Highlight regions
(setq line-number-mode t)              ; Display line numbers
(setq column-number-mode t)            ; Display column numbers
(blink-cursor-mode 0)                  ; Don't blink the cursor
(server-start)                         ; Start the Emacs server
(setq visible-bell t)                  ; Disable beep
(setq confirm-kill-emacs 'yes-or-no-p) ; Confirm quit
;(setq sentence-end-double-space nil)   ; Wrap lines after only one space

(setq c-basic-offset 4)                ; Cause tab key to indent 4 places
(setq tab-width 4)                     ; Interpret tab char as 4 places
(setq-default indent-tabs-mode nil)    ; Insert spaces instead of tabs

(add-to-list 'load-path "~/.emacs.d/site-lisp")

;; Put all backup files in a single directory
(defun my-backup-file-name (fpath)
 "Return a new file path of a given file path. If the new path's directory
 does not exist, create them."
 (let (backup-root bpath)
   (setq backup-root "~/.emacs.d/backup")
   (setq bpath (concat backup-root fpath "~"))
   (make-directory (file-name-directory bpath) bpath)
   bpath))

;; Since this function doesn't work in Windows, I can't enable it
;; here.  Instead, copy this function into your local .emacs file:
;; (setq make-backup-file-name-function 'my-backup-file-name)

;; -----------------------------------------------------------------------------
;; Color themes

(require 'color-theme)
(color-theme-initialize)
(color-theme-almost-monokai)

;; -----------------------------------------------------------------------------
;; org-mode

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-ca" 'org-agenda)
(setq org-M-RET-may-split-line nil)
(setq org-log-done 'time)

;; -----------------------------------------------------------------------------
;; Programming language modes

;; Clojure
(require 'clojure-mode)

;; Erlang
(require 'erlang-start)

;; Go
(require 'go-mode-load)

;; JavaScript
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Markdown
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
