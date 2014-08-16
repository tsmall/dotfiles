;;; emacs.el --- Tom's Emacs Customizations
;;
;; Put these lines in your ~/.emacs file to load this file and all of its
;; libraries:
;;
;;   (add-to-list 'load-path "~/etc/dotfiles/emacs.d/site-lisp")
;;   (load "~/etc/dotfiles/emacs.el")

;;; Code:

(add-to-list 'load-path "~/etc/dotfiles/emacs.d/settings")
(require 'eshell-settings)
(require 'font-settings)
(require 'mac-settings)
(require 'org-settings)

;; -----------------------------------------------------------------------------
;; General customizations
;; -----------------------------------------------------------------------------

(add-to-list 'default-frame-alist '(height . 35))
(add-to-list 'default-frame-alist '(width . 100))

(setq transient-mark-mode t)           ; Highlight regions
(setq line-number-mode t)              ; Display line numbers
(setq column-number-mode t)            ; Display column numbers
(blink-cursor-mode 0)                  ; Don't blink the cursor
(server-start)                         ; Start the Emacs server
(setq visible-bell t)                  ; Disable beep
(setq confirm-kill-emacs 'yes-or-no-p) ; Confirm quit
(tool-bar-mode -1)                     ; Disable tool bar
(set-scroll-bar-mode nil)              ; Hide the scroll bars
(ido-mode t)                           ; Turn on ido-mode
(fset 'yes-or-no-p 'y-or-n-p)          ; Make answering yes or no easier

(setq c-basic-offset 4)                ; Cause tab key to indent 4 places
(setq sgml-basic-offset 4)             ; Also use 4 spaces for HTML
(setq tab-width 4)                     ; Interpret tab char as 4 places
(setq-default indent-tabs-mode nil)    ; Insert spaces instead of tabs

;; "Uniquify" buffers
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; -----------------------------------------------------------------------------
;; Custom functions
;; -----------------------------------------------------------------------------

(defun eshell/clear ()
  "Clear the eshell buffer, like 'clear' in bash."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun my-backup-file-name (fpath)
  "Return a new file path of a given FPATH.
If the new path's directory does not exist, create them."
  (let (backup-root bpath)
    (setq backup-root "~/.emacs.d/backup")
    (setq bpath (concat backup-root fpath "~"))
    (make-directory (file-name-directory bpath) bpath)
    bpath))

;; Since this function doesn't work in Windows, I can't enable it
;; here.  Instead, copy this function into your local .emacs file:
;; (setq make-backup-file-name-function 'my-backup-file-name)

(defun clear-buffer ()
  "Clear the contents of the current buffer."
  (interactive)
  (delete-region 1 (point-max)))

(defun insert-comment-line ()
  "Insert '-' characters from point to column 80."
  (interactive)
  (insert (make-string (- 80 (current-column)) ?-)))

(defun trs-copy-buffer-to-clipboard ()
  "Copy the entire contents of the current buffer to the clipboard."
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max))
  (message "Copied."))

;; Taken verbatim from the Emacs wiki:
;; http://www.emacswiki.org/emacs/IncrementNumber
(defun trs-increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))

(defun trs-count-matches-in-line (regexp)
  "Search for all REGEXP matches in the current line.
Display the number of matches and save it to the kill ring."
  (interactive "sRegexp to match: ")
  (let ((matches (count-matches regexp (line-beginning-position) (line-end-position))))
    (message "Matches: %s" matches)
    (kill-new (format "%s" matches))))

(defun trs-goto-buffer (buffer-name file-path)
  "Go to buffer in current window, loading it from FILE-PATH if it's not loaded."
  (let ((buffer (get-buffer buffer-name)))
    (if (not buffer)
        (setq buffer (find-file-noselect file-path)))
    (switch-to-buffer buffer)))

(require 'pomodoro)
(require 'util)

;; -----------------------------------------------------------------------------
;; Custom key bindings
;; -----------------------------------------------------------------------------

(global-set-key (kbd "<f8>") 'deft)
(global-set-key (kbd "<f9>") 'eshell)

(global-set-key (kbd "C-c [") 'flymake-goto-prev-error)
(global-set-key (kbd "C-c ]") 'flymake-goto-next-error)
(global-set-key (kbd "C-c \\") 'flymake-display-err-menu-for-current-line)
(global-set-key (kbd "C-c -") 'insert-comment-line)
(global-set-key (kbd "C-c #") 'trs-count-matches-in-line)
(global-set-key (kbd "C-c i") 'trs-increment-number-decimal)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'bury-buffer)
(global-set-key (kbd "C-c d") 'pgg-decrypt-region)
(global-set-key (kbd "C-c e") 'pgg-encrypt-symmetric-region)
(global-set-key (kbd "C-c l") 'clear-buffer)
(global-set-key (kbd "C-c w") 'trs-copy-buffer-to-clipboard)

;; Scrolling
(global-set-key (kbd "C-S-n") 'scroll-up-line)
(global-set-key (kbd "C-S-p") 'scroll-down-line)

;; pomodoro
(global-set-key (kbd "C-c p p") 'pomodoro-start)
(global-set-key (kbd "C-c p s") 'pomodoro-start-short-break)
(global-set-key (kbd "C-c p l") 'pomodoro-start-long-break)
(global-set-key (kbd "C-c p r") 'pomodoro-remaining-time)

;; -----------------------------------------------------------------------------
;; Major modes
;; -----------------------------------------------------------------------------

;; Deft
(add-to-list 'load-path "~/etc/dotfiles/emacs.d/site-lisp/deft")
(require 'deft)
(setq deft-use-filename-as-title t)

;; Diff
(require 'diff-mode)
(set-face-attribute 'diff-added nil :foreground "green3")
(set-face-attribute 'diff-removed nil :foreground "red3")
(set-face-attribute 'diff-changed nil :foreground "purple")

;; pcomplete
(add-to-list 'load-path "~/etc/dotfiles/emacs.d/site-lisp/pcmpl-git-el")
(require 'pcmpl-git)

;; transpose-frame
(require 'transpose-frame)

;; -----------------------------------------------------------------------------
;; Package Configuration
;; -----------------------------------------------------------------------------

(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)
(setq package-enable-at-startup nil)

(ignore-errors
  ;; caml-mode
  (setq auto-mode-alist
        (cons '("\\.ml[iylp]?$" . caml-mode) auto-mode-alist))
  (autoload 'caml-mode "caml" "Major mode for editing Caml code." t)
  (autoload 'run-caml "inf-caml" "Run an inferior Caml process." t)
  (if window-system (require 'caml-font))

  ;; haskell-mode
  (require 'haskell-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

  ;; jabber
  (require 'jabber)
  (set-face-attribute 'jabber-roster-user-away nil :foreground "royal blue")
  (set-face-attribute 'jabber-roster-user-online nil :foreground "deep sky blue" :weight 'normal)
  (set-face-attribute 'jabber-chat-prompt-local nil :foreground "royal blue")
  (setq jabber-chat-fill-long-lines nil)
  (add-hook 'jabber-chat-mode-hook
            (lambda ()
              (toggle-word-wrap)
              (turn-on-flyspell)))

  ;; js2-mode
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))

;;; emacs.el ends here
