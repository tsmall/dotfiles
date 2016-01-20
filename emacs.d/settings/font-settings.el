;;; font-settings.el --- Font Settings

;;; Commentary:

;; Set the fonts Emacs uses. I'm a bit nuts about fonts, and tend to change my
;; preferred font quite often. So I made it easy for me to switch between them.

;;; Code:

(defun font-settings-set-default-font (family &optional height weight)
  "Set the default font."
  (set-face-attribute 'default nil
                      :family family
                      :height (or height 100)
                      :weight (or weight 'medium)))

(defun font-settings-set-variable-font (family &optional height weight)
  "Set the variable width font."
  (set-face-attribute 'variable-pitch nil
                      :family family
                      :height (or height 100)
                      :weight (or weight 'medium)))

(provide 'font-settings)

;;; font-settings.el ends here
