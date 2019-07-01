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

(load "~/etc/dotfiles/emacs.d/init-packages")

;; Eventually the emacs.lp.el file will replace this one. But since migrating
;; all of my code into emacs.org is going to take a little while, I'll load
;; that file along with this one. That way I can migrate it piece-by-piece.
(load-file "~/etc/dotfiles/emacs.lp.el")

;; -----------------------------------------------------------------------------
;; Custom functions
;; -----------------------------------------------------------------------------

(defun eshell/cl ()
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

(defun trs-name-to-email (name)
  "Convert the string NAME to an email address."
  (let* ((parts (->> name
                     downcase
                     split-string))
         (first (->> (car parts)
                     string-to-char
                     string))
         (last (cadr parts))
         (domain "@schoology.com"))
    (concat first last domain)))

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
(global-set-key (kbd "C-c #") 'trs-count-matches-in-line)
(global-set-key (kbd "C-c b") 'bury-buffer)
(global-set-key (kbd "C-c d") 'pgg-decrypt-region)
(global-set-key (kbd "C-c e") 'pgg-encrypt-symmetric-region)

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; org-mode
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

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
;; jabber-mode
;; -----------------------------------------------------------------------------

(ignore-errors
  ;; jabber
  (require 'jabber)
  (set-face-attribute 'jabber-roster-user-away nil :foreground "royal blue")
  (set-face-attribute 'jabber-roster-user-online nil :foreground "deep sky blue" :weight 'normal)
  (set-face-attribute 'jabber-chat-prompt-local nil :foreground "royal blue")
  (setq jabber-chat-fill-long-lines nil)
  (add-hook 'jabber-chat-mode-hook
            (lambda ()
              (toggle-word-wrap)
              (turn-on-flyspell))))

;;; emacs.el ends here
