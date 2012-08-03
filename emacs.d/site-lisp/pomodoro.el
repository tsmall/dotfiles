;;; pomodoro.el --- A Pomodoro timer in Emacs-Lisp

;; Copyright (C) 2011 Tom Small <tsmall3@gmail.com>

;; Author: Tom Small <tsmall3@gmail.com>
;; Created: 29 Nov 2011

;;; Commentary:
;; 
;; This library implements a Pomodoro timer for Emacs in Emacs-Lisp.  It uses
;; either Ubuntu's d-bus based message notification system, Mac OS X's Growl
;; notification system, or Emacs's notification system to indicate when a timer
;; has ended.
;;
;; This library doesn't define any key mappings, but these are recommended:
;;
;;   (global-set-key "\C-cpp" 'pomodoro-start)
;;   (global-set-key "\C-cps" 'pomodoro-start-short-break)
;;   (global-set-key "\C-cpl" 'pomodoro-start-long-break)
;;   (global-set-key "\C-cpr" 'pomodoro-remaining-time)

;;; To-do
;; - Show remaining time in the mode line

;;; Code:

(require 'notifications)

(defvar pomodoro-length "25 min"
  "Length of time for a single pomodoro.")

(defvar pomodoro-short-break-length "5 min"
  "Length of time for a short break.")

(defvar pomodoro-long-break-length "15 min"
  "Length of time for a long break.")

(defvar pomodoro-current-timer nil
  "The current timer.")

(defvar pomodoro-current-message nil
  "The current timer's end message.")

(defun pomodoro-start ()
  "Start a new pomodoro timer."
  (interactive)
  (pomodoro-set-timer pomodoro-length "Time's up!")
  (message "Pomodoro started."))

(defun pomodoro-start-short-break ()
  "Start a new short break timer."
  (interactive)
  (pomodoro-set-timer pomodoro-short-break-length "Break's over!")
  (message "Short break timer started."))

(defun pomodoro-start-long-break ()
  "Start a new long break timer."
  (interactive)
  (pomodoro-set-timer pomodoro-long-break-length "Break's over!")
  (message "Long break timer started."))

(defun pomodoro-stop ()
  "Stop the current timer."
  (interactive)
  (if (not (equal pomodoro-current-timer nil))
      (cancel-timer pomodoro-current-timer))
  (setq pomodoro-current-timer nil))

(defun pomodoro-remaining-time ()
  "Show the remaining time in the current timer."
  ;; This code is shamelessly copied from org-timer.el.
  (interactive)
  (if (equal pomodoro-current-timer nil)
      (message "No timer running")
    (let* ((rtime (decode-time
                   (time-subtract (timer--time pomodoro-current-timer)
                                  (current-time))))
           (rsecs (nth 0 rtime))
           (rmins (nth 1 rtime)))
      (message "%d minute(s) %d second(s) left in current pomodoro"
               rmins rsecs))))

(defun pomodoro-set-timer (length end-msg)
  "Start new timer that runs for LENGTH and shows END-MSG when done."
  (pomodoro-stop)
  (setq pomodoro-current-message end-msg)
  (setq pomodoro-current-timer (run-at-time length nil 
                                            (lambda () (progn (pomodoro-show-message pomodoro-current-message)
                                                              (setq pomodoro-current-timer nil))))))
(defun pomodoro-show-message (msg)
  "Show the MSG string to the user."
  (cond ((string= system-type "gnu/linux") (pomodoro-show-dbus-message msg))
        ((string= system-type "darwin") (pomodoro-show-growl-message msg))
        (t (message msg))))

(defun pomodoro-show-dbus-message (msg)
  "Show the MSG string to the user via Ubuntu's notification service."
  (notifications-notify :title "Pomodoro"
                        :body msg))

(defun pomodoro-show-growl-message (msg)
  "Show the MSG string to the user via Growl."
  (start-process "growl" nil "growlnotify"
                 "-a" "Emacs"
                 "-t" "Pomodoro"
                 "-m" msg))

(provide 'pomodoro)

;;; pomodoro.el ends here
