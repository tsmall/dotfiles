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

(provide 'offerpop)

;;; offerpop.el ends here
