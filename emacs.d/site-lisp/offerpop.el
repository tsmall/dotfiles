;;; offerpop.el --- Custom code for working at Offerpop

;; Copyright (C) 2011 Tom Small <tom@offerpop.com>

;; Author: Tom Small <tom@offerpop.com>
;; Created: 18 Oct 2011

;;; Commentary:
;; 
;; This library contains functions that I've found helpful while working at
;; Offerpop.  None of the functions are bound to keyboard shortcuts; you've got
;; to do that in a way that makes sense in your .emacs file.

;;; Code:

(defun op-restart-apache ()
  "Restart the local Apache server."
  (interactive)
  (op--run-sudo-command "service apache2 restart")
  (message "Successfully restarted apache."))

(defun op-restart-memcached ()
  "Restart the local Apache server."
  (interactive)
  (op--run-sudo-command "service memcached restart")
  (message "Successfully restarted memcached."))

(defun op--run-sudo-command (command)
  "Run a shell command that requires sudo access.

Note that you have to have sudo set up to not require a password
for the command that you want to run, as this function won't prompt
the user for their password."
  (let ((sudo-command (concat "sudo " command)))
    (start-process command "*op-sudo-output*"
                   "bash" "-c" sudo-command)))

(defun op-comment-line (regexp comment-string)
  "Comment the first line in the current buffer matching REGEXP with COMMENT-CHAR."
  (goto-char (point-min))
  (re-search-forward regexp)
  (beginning-of-line)
  (insert comment-string))

(defun op-uncomment-line (regexp comment-string)
  "Uncomment the first line in the current buffer matching REGEXP."
  (goto-char (point-min))
  (re-search-forward regexp)
  (beginning-of-line)
  (delete-region (point) (+ (point) (length comment-string))))

(defun op-change-host (hostname)
  "Change the host in my hosts file from current to HOSTNAME."
  (interactive "sSwitch to host: ")
  (save-current-buffer
    (set-buffer (get-buffer "hosts"))
    (op-comment-line "^[^#][[:digit:]].+offerpop\.com" "#")
    (op-uncomment-line (format "# %s$" hostname) "#")
    (save-buffer)
    (message (format "Successfully switched to %s." hostname))))

(defun op-open-or-switch-to-mysql-buffer ()
  "Switch to the *mysql* buffer, creating it if it doesn't exist."
  (interactive)
  (let ((mysql-buffer (get-buffer "*mysql*")))
    (if (not mysql-buffer)
        (progn
          (setf mysql-buffer (generate-new-buffer "*mysql*"))
          (shell mysql-buffer)
          (sleep-for 1)
          (with-current-buffer mysql-buffer
            (toggle-truncate-lines)
            (goto-char (point-max))
            (insert "cd; db local")
            (comint-send-input))))
    (switch-to-buffer mysql-buffer)))

(defun op-open-or-switch-to-log-buffer ()
  "Switch to the *log* buffer, creating it if it doesn't exist."
  (interactive)
  (let ((log-buffer (get-buffer "*logs*")))
    (if (not log-buffer)
        (progn
          (setf log-buffer (generate-new-buffer "*logs*"))
          (shell log-buffer)
          (sleep-for 1)
          (with-current-buffer log-buffer
            (toggle-truncate-lines)
            (hi-lock-line-face-buffer "-->" 'hi-yellow)
            (hi-lock-line-face-buffer "ERROR" 'hi-red-b)
            (goto-char (point-max))
            (insert "cd ~/Projects/Offerpop/Log; tail -fn 0 offerpop.log")
            (comint-send-input))))
    (switch-to-buffer log-buffer)))

(defun op-setup-for-code-review ()
  "Configure the current buffer's environment for code review."
  (interactive)
  (linum-mode)
  (hl-line-mode)
  (cd "~/Projects/Offerpop"))

(defun op-setup-svn-incoming ()
  "Configure the current buffer's environment for svn-incoming output."
  (interactive)
  (let ((rev-regexp "r[0-9]+")
        (rm-regexp "\\(RM\\|rm\\)[^0-9\n]*[0-9]+"))
    ;; Remove any highlighting that may already be there.
    (hi-lock-unface-buffer rev-regexp)
    (hi-lock-unface-buffer rm-regexp)

    ;; Now set it up.
    (hl-line-mode 1)
    (hi-lock-face-buffer rev-regexp 'hi-yellow)
    (hi-lock-face-buffer rm-regexp 'hi-blue)))

;;; Redmine

(defvar op-redmine-base-url "http://redmine.offerpop.com/"
  "Base URL for Offerpop's Redmine site.")

(defun op-redmine-goto-issue ()
  "Go to the Redmine issue identified by the number at point."
  (interactive)
  (let ((issue-num (op-redmine-parse-issue-word (word-at-point))))
    (browse-url (op-redmine-issue-url issue-num))))

(defun op-redmine-parse-issue-word (word)
  "Return just the issue number from the Redmine issue WORD."
  (if (string-prefix-p "rm" word t)
      (substring word 2)
    word))

(defun op-redmine-issue-url (issue-num)
  "Return URL to Redmine issue with number ISSUE-NUM."
  (let ((issue-num-str (if (numberp issue-num)
                           (number-to-string issue-num)
                         issue-num)))
    (concat op-redmine-base-url "issues/" issue-num-str)))

;;; Django, Celery, etc.

(defun op-open-or-switch-to-django-buffer ()
  "Switch to the *django* buffer, creating it if it doesn't exist.

The *django* buffer is used to run the Django development server in
a shell.  Running it an Emacs shell buffer instead of a terminal
makes it possible to easily control it with simple keyboard shortcuts
and to get a full view of its log."
  (interactive)
  (let ((commands '("cd ~/Projects/Offerpop"
                    "dashenv"
                    "python dashboard/manage.py runserver")))
    (op--open-or-switch-to-shell-buffer "*django*" commands)))

(defun op-open-or-switch-to-celery-buffer ()
  "Switch to the *celery* buffer, creating it if it doesn't exist.

The *celery* buffer is used to host the Celery distributed message
queue daemon in a shell."
  (interactive)
  (let ((commands '("cd ~/Projects/Offerpop"
                    "dashenv"
                    "python dashboard/manage.py celery worker -l INFO")))
    (op--open-or-switch-to-shell-buffer "*celery*" commands)))

(defun op--open-or-switch-to-shell-buffer (buffer-name commands)
  (let ((buffer (get-buffer buffer-name)))
    (if (not buffer)
        (progn
          (setf buffer (generate-new-buffer buffer-name))
          (shell buffer)
          (sleep-for 1)
          (with-current-buffer buffer
            (toggle-truncate-lines)
            (goto-char (point-max))
            (dolist (command commands)
              (op--run-shell-command command))))
      (switch-to-buffer buffer))))

(defun op--run-shell-command (command)
  "Run COMMAND in the active shell (comint) buffer."
  (insert command)
  (comint-send-input))

(provide 'offerpop)

;;; offerpop.el ends here
