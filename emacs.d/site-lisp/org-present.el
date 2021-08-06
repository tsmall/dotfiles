;;; org-present.el --- Helpers for presenting in Org Mode

;; Author: Tom Small <tsmall3@gmail.com>
;; Created: 27 Jul 2021


;;; Commentary:
;;
;; These functions were helpfully shared by Tyler on the Emacs
;; Stackexchange: <https://emacs.stackexchange.com/a/64144>
;;
;; I tweaked them slightly to follow my function naming scheme.

;;; Usage:
;;
;; In and org-mode file, start presenting by running this command:
;;
;;   M-x org-demo-minor-mode

;;; Code:

(defun trs/present ()
  (interactive)
  (org-narrow-to-subtree))

(defun trs/present-next (ARG)
  (interactive "p")
  (beginning-of-buffer)
  (widen)
  (condition-case nil
      (outline-forward-same-level ARG)
    (error (message "No more headings at this level")))
  (org-narrow-to-subtree))

(defun trs/present-previous (ARG)
  (interactive "p")
  (beginning-of-buffer)
  (widen)
  (condition-case nil
      (outline-backward-same-level ARG)
    (error (message "No more headings at this level")))
  (org-narrow-to-subtree))

(defun trs/present-up (ARG)
  (interactive "p")
  (beginning-of-buffer)
  (widen)
  (condition-case nil
      (outline-up-heading ARG)
    (error (message "Already at top level")))
  (org-narrow-to-subtree))

(defvar org-demo-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<down>") 'trs/present-next)
    (define-key map (kbd "C-<up>") 'trs/present-previous)
    (define-key map (kbd "C-S-<up>") 'trs/present-up)
    (define-key map (kbd "C-c C-s") 'trs/present)
    (define-key map (kbd "C-c C-q") 'org-demo-minor-mode)
    map))

(define-minor-mode org-demo-minor-mode
  "A minor mode for presenting org files as slides.
Very basic - narrows to current subtree, and provides navigation keys for
moving forwards and backwards through the file. Should probably also allow
for increasing font size?"
  nil nil
  :lighter " DEMO"
  (if org-demo-minor-mode
      (org-narrow-to-subtree)
    (widen)))

;;; org-present.el ends here
