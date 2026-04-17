;;; org-custom-faces.el --- Custom faces for org-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Tom Small

;; Author: Tom Small <tsmall3@proton.me>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;; Hour Backgound Faces

(defface type-morning
  '((t (:inherit default :background "lightgreen")))
  "Face for morning times.")

(defface type-work
  '((t (:inherit default :background "lightblue")))
  "Face for work times.")

(defface type-family
  '((t (:inherit default :background "lightpink")))
  "Face for family times.")

(defface type-evening
  '((t (:inherit default :background "lightgreen")))
  "Face for evening times.")

;; People's Faces

(defface type-grace
  '((t (:inherit default :foreground "lightblue")))
  "Face for Grace's events.")

(defface type-me
  '((t (:inherit default :foreground "orange")))
  "Face for my events.")

(defface type-wife
  '((t (:inherit default :foreground "gold")))
  "Face for Teresa's events.")

;; Other Faces

(defface type-birthday
  '((t (:inherit default :foreground "#28CD41" :slant italic)))
  "Face for birthdays.")

(defface type-date
  '((t (:inherit default :foreground "blue")))
  "Face for dates.")

(defface type-due
  '((t (:inherit default :foreground "red" :weight bold)))
  "Face for due dates.")

(defface type-highlight
  '((t (:inherit default :background "#FFF085")))
  "Face for highlighting text.")

(defface type-holiday
  '((t (:inherit default :foreground "#82B6FA" :slant italic)))
  "Face for holidays.")

(defface type-info
  '((t (:inherit default :foreground "darkgray")))
  "Face for info.")

(defface type-note
  '((t (:inherit default :foreground "darkgray" :slant italic :height 0.9)))
  "Face for notes.")

(defface type-rpt
  '((t (:inherit default :foreground "gray40")))
  "Face for repeating tasks.")

(defface type-sunday
  '((t (:inherit default :foreground "blue" :weight bold)))
  "Face for Sundays.")

(defface type-tag
  '((t (:inherit default :foreground "lightgray" :slant italic :height 0.9)))
  "Face for tags.")

(defface type-time
  '((t (:inherit default :foreground "blue")))
  "Face for time.")

;; Custom Org Link Logic

(org-link-set-parameters
 "type"
 :follow (lambda (path)
           (message "You followed a link of type 'type' with path: %s" path))
 :face (lambda (path)
         (let ((face-name (intern (concat "type-" path))))
           (if (facep face-name)
               face-name
             'note))))

(provide 'org-custom-faces)
;;; org-custom-faces.el ends here
