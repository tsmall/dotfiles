;;; util.el --- Small utility functions

;; Copyright (C) 2012 Tom Small <tsmall3@gmail.com>

;; Author: Tom Small <tsmall3@gmail.com>
;; Created: 27 Jun 2012


;;; Commentary:
;;
;; This is just a collection of utility functions that I have found useful at
;; one time or another.

;;; Code:

(defun kb->b (kb)
  "Convert KB to bytes."
  (* kb 1024))

(defun mb->kb (mb)
  "Convert MB to kilobytes."
  (* mb 1024))

(defun mb->b (mb)
  "Convert MB to bytes."
  (kb->b (mb->kb mb)))


(defun b->kb (b)
  "Convert B to kilobytes."
  (/ b 1024.0))

(defun kb->mb (kb)
  "Convert KB to megabytes."
  (/ kb 1024.0))

(defun b->mb (b)
  "Convert B to megabytes."
  (kb->mb (b->kb b)))

(provide 'util)

;;; util.el ends here
