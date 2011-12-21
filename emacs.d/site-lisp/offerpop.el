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
  "Restart the local Apache server.

Assumes that there is a shell buffer named '*shell:root*'
that is already running as root, which is used to actually
run the command."
  (interactive)
  (save-current-buffer
    (set-buffer (get-buffer "*shell:root*"))
    (goto-char (point-max))
    (insert "service apache2 restart")
    (comint-send-input))
  (message "Successfully restarted apache."))

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
            (insert "cd; mysql-local")
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
            (insert "cd ~/Projects/Offerpop/Log; tail -fn 0 log.txt")
            (comint-send-input))))
    (switch-to-buffer log-buffer)))

(provide 'offerpop)

;;; offerpop.el ends here
