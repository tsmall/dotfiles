;;; gwd-trivia.el --- GWD Trivia Analysis -*- lexical-binding:t; sentence-end-double-space:t -*-

;; Copyright (C) 2022 Tom Small

;; Author: Tom Small <tsmall3@proton.me>
;; Created: 15 Aug 2022
;; Version: 0.0.1
;; Keywords: trivia, analysis

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
;; This file provides a single command, gwd-analyze, which analyzes the current
;; buffer.  It expects that buffer to contain the data from Geeks Who Drinks
;; trivias, in a specific format.  The results of the analyses are printed to a
;; new buffer.

;;; Code:

(defun gwd--parse-header ()
  ;; TODO Documentation string
  (let ((header (make-hash-table))
        line)
    ;; TODO Replace this with more general "skip whitespace" logic.
    (forward-line 2)
    (while (progn
             (forward-line)
             (not (looking-at "^$")))
      (cond
       ((looking-at "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
        (puthash 'date (match-string 0) header))
       ((looking-at "Total (7): \\([0-9]+\\)")
        (puthash 'score (string-to-number (match-string 1)) header)
        (puthash 'rounds 7 header))
       ((looking-at "Total: \\([0-9]+\\)")
        (puthash 'score (string-to-number (match-string 1)) header)
        (puthash 'rounds 8 header))
       ((looking-at "Team: \\(.+\\)$")
        (puthash 'team (match-string 1) header))
       ((looking-at "QM: \\(.+\\)$")
        (puthash 'qm (match-string 1) header))))
    header))

(defun gwd--parse ()
  ;; TODO Documentation string
  (save-excursion
    (goto-char (point-min))
    (let ((headers '()))
      (while (search-forward "-----" nil t)
        (setq headers (cons (gwd--parse-header) headers)))
      headers)))

(defun gwd-analyze ()
  ;; TODO Documentation string
  (interactive)
  (let ((headers (gwd--parse))
        (game-count 0)
        (best-score 0)
        (best-date "")
        (best-team "")
        (worst-score most-positive-fixnum)
        (worst-date "")
        (worst-team "")
        (teams '())
        (team-counts '())
        (best-by-team '())
        (worst-by-team '())
        (qm-counts '()))
    (dolist (header headers)
      (if-let ((score (gethash 'score header))
               (team  (gethash 'team  header)))
          (when (= (gethash 'rounds header) 7)
            (progn
              (incf game-count)
              (incf (alist-get team team-counts 0 nil #'equal))
              (when (> score best-score)
                (setq best-score score)
                (setq best-date (gethash 'date header))
                (setq best-team team))
              (when (< score worst-score)
                (setq worst-score score)
                (setq worst-date (gethash 'date header))
                (setq worst-team team))
              (when (not (member team teams))
                (add-to-list 'teams team))
              (when (> score (alist-get team best-by-team 0 nil #'equal))
                (setf (alist-get team best-by-team nil nil #'equal) score))
              (when (< score (alist-get team worst-by-team most-positive-fixnum nil #'equal))
                (setf (alist-get team worst-by-team nil nil #'equal) score))
              (if-let ((qm (gethash 'qm header)))
                  (incf (alist-get qm qm-counts 0 nil #'equal)))))))
    (with-output-to-temp-buffer "*gwd:analysis*"
      (princ "GWD Analysis\n")
      (princ "============\n\n")
      (princ "(Including 7 round games only.)\n")
      (princ "\n\n")
      (princ "Summary\n")
      (princ "-------\n\n")
      (princ (format "Games Played: %d\n" game-count))
      (princ (format "Best Score:   %d ('%s' on %s)\n"
                     best-score best-team best-date))
      (princ (format "Worst Score:  %d ('%s' on %s)\n"
                     worst-score worst-team worst-date))
      (princ "\n\n")
      (princ "Teams\n")
      (princ "-----\n\n")
      (dolist (team (sort teams #'string<))
        (princ (format "%2d %2d %2d   %s\n"
                       (alist-get team team-counts)
                       (alist-get team best-by-team)
                       (alist-get team worst-by-team)
                       team)))
      (princ "\n\n")
      (princ "QMs\n")
      (princ "---\n\n")
      (dolist (qm (sort (mapcar #'car qm-counts) #'string<))
        (princ (format "%2d   %s\n"
                       (alist-get qm qm-counts)
                       qm))))))

(provide 'gwd-trivia)

;;; gwd-trivia.el ends here
