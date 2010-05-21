;; -----------------------------------------------------------------------------
;; General customizations

(setq transient-mark-mode t)           ; Highlight regions
(setq line-number-mode t)              ; Display line numbers
(setq column-number-mode t)            ; Display column numbers
(blink-cursor-mode 0)                  ; Don't blink the cursor
(server-start)                         ; Start the Emacs server
(setq default-tab-width 4)
(setq tab-width 4)
(setq visible-bell t)                  ; Disable beep
(setq confirm-kill-emacs 'yes-or-no-p) ; Confirm quit
(setq sentence-end-double-space nil)   ; Wrap lines after only one space

(add-to-list 'load-path "~/.emacs.d/site-lisp")

;; Put all backup files in a single directory
;(defun my-backup-file-name (fpath)
;  "Return a new file path of a given file path. If the new path's directory
;  does not exist, create them."
;  (let (backup-root bpath)
;	(setq backup-root "~/.emacs.d/backup")
;	(setq bpath (concat backup-root fpath "~"))
;	(make-directory (file-name-directory bpath) bpath)
;	bpath))
;(setq make-backup-file-name-function 'my-backup-file-name)

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
;; Twitter

(autoload 'twit-show-recent-tweets	"twit" "" t) ; most recent direct tweets (!)
(autoload 'twit-show-at-tweets		"twit" "" t) ; directed to you
(autoload 'twit-show-friends 		"twit" "" t) ; your friends
(autoload 'twit-show-followers 		"twit" "" t) ; your followers

(autoload 'twit-follow-recent-tweets	"twit" "" t) ; at idle, check at background

(autoload 'twit-post			"twit" "" t)
(autoload 'twit-post-region		"twit" "" t)
(autoload 'twit-post-buffer		"twit" "" t)
(autoload 'twit-direct			"twit" "" t) ; tweet to person

(autoload 'twit-add-favorite		"twit" "" t) ; Add to favourite: (*) star
(autoload 'twit-remove-favorite 	"twit" "" t)

(autoload 'twit-add-friend  		"twit" "" t) ; follow a friend
(autoload 'twit-remove-friend 		"twit" "" t) ; emove a frienda

;; -----------------------------------------------------------------------------
;; Programming language modes

;; Erlang
(require 'erlang-start)

;; Go
(require 'go-mode-load)

;; JavaScript
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
