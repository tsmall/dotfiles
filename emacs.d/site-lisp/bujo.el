;;; bujo.el --- Bullet Journal Utilities -*- lexical-binding:t; sentence-end-double-space:t -*-

;; Copyright (C) 2022 Tom Small

;; Author: Tom Small <tsmall3@proton.me>
;; Created: 24 Aug 2022
;; Version: 0.0.1
;; Keywords: notes, bujo

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This file provides helper functions for working with my simple Bullet
;; Journal like text file format.

;;; Code:

(defvar bujo--date-line-regex
  "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} \\(?:Sun\\|Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\)")

(defun bujo-property-report (property-name)
  (interactive "sProperty: ")
  (let ((prop-regex (format "%s: \\(.+\\)" property-name))
        (entries '()))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "---" nil t)
        (forward-line 2)
        (when (looking-at bujo--date-line-regex)
          (let ((current-date (match-string 0)))
            (forward-line)
            (while (progn
                     (forward-line)
                     (not (looking-at "^$")))
              (when (looking-at prop-regex)
                (let ((value (match-string 1)))
                  (push (list current-date value) entries))))))))
    (with-output-to-temp-buffer "*bujo:property-report*"
      (princ "BuJo Property Report\n")
      (princ "--------------------\n")
      (princ "\n")
      (princ (format "Property: \"%s\"" property-name))
      (princ "\n\n")
      (dolist (each entries)
        (princ (format "%s - %s\n" (car each) (cadr each)))))))

(defun bujo-timesheet ()
  (interactive)
  (bujo-property-report "Time"))

(provide 'bujo)

;;; bujo.el ends here
