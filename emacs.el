(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(tool-bar-mode nil))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :family "apple-monaco")))))

;; -----------------------------------------------------------------------------

;; General customizations
(setq transient-mark-mode t) ;; Highlight regions
(setq line-number-mode t) ;; Display line numbers
(setq column-number-mode t) ;; Display column numbers
(blink-cursor-mode 0) ;; Don't blink the cursor
(server-start) ;; Start the Emacs server
(setq default-tab-width 4)
(setq tab-width 4)
(setq visible-bell t) ;; Disable beep
(setq confirm-kill-emacs 'yes-or-no-p) ; Confirm quit

;; Turn off anti-aliasing
;; Note: To actually not use anti-aliasing I had to also run this command:
;;   defaults write org.gnu.Emacs AppleAntiAliasingThreshold 100
(setq mac-allow-anti-aliasing nil)

(add-to-list 'load-path "~/.emacs.d/site-lisp")

;; -----------------------------------------------------------------------------
;; org-mode

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-ca" 'org-agenda)

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

;; Erlang
(require 'erlang-start)

;; Go
(require 'go-mode-load)

;; JavaScript
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
