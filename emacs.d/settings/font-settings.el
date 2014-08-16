;;; font-settings.el --- Font Settings

;;; Commentary:

;; Set the fonts Emacs uses. I'm a bit nuts about fonts, and tend to change my
;; preferred font quite often. That's why there are so many here.

;;; Code:

(defun font-settings-set-default-font (family &optional height)
  "Set the default font."
  (set-face-attribute 'default nil :family family :height (or height 100)))

(defun font-settings-set-variable-font (family &optional height)
  "Set the variable width font."
  (set-face-attribute 'variable-pitch nil :family family :height (or height 100)))

(provide 'font-settings)

;;; font-settings.el ends here
